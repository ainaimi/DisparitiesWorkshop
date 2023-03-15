packages <- c("data.table","tidyverse","skimr","here", "lmtest", "sandwich", "VGAM")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}

## read in the data

a <- read_csv(here("data","nhefs_analytic_data.csv"))

head(a)
tail(a)

## start with some basic regression

## outcome model

names(a)

outcome_model_add <- lm(hbp ~ race + income + race*income + marital + age + school, data = a)
summary(outcome_model_add)

outcome_model_mult <- glm(hbp ~ race + income + race*income + marital + age + school, 
                         family = binomial("logit"),
                         data = a)
summary(outcome_model_mult)

## propensity score model exposure (no confounders for race)

## propensity score model mediator (income)

ps_model_m <- vglm(income_cat ~ race + marital + age + school, 
                   data = a, family = multinomial)
summary(ps_model_m)

# let's look at the fitted values (propensity score) from this model
head(ps_model_m@fitted.values)

## create an index to pick off the right PS column
a <- a %>% mutate(index = as.numeric(income_cat == "(17,20]")*1 +
                    as.numeric(income_cat == "(20,22]")*2 +
                    as.numeric(income_cat == "[11,17]")*3)

head(
  cbind(a,
        ps_model_m@fitted.values)
     )

a$propensity_score <- NA
for(i in 1:nrow(a)){
  a[i,]$propensity_score <- ps_model_m@fitted.values[i,a[i,]$index]
}

head(
  cbind(a,
        ps_model_m@fitted.values)
)

# let's look at the ps overlap

plot_dat <- tibble(
  PS = a$propensity_score,
  Income = factor(a$income_cat)
)

ggplot(plot_dat) +
  geom_density(aes(PS, 
                   group = Income, 
                   color = Income),
               kernel = "epanechnikov",
               bw = .1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()

ggsave(here("figures","ps_overlap_income.pdf"))

# construct mediator weights

a <- a %>%  mutate(sw_income = (mean(index==1)/propensity_score)*(index==1) +
                     (mean(index==2)/propensity_score)*(index==2) +
                      (mean(index==3)/propensity_score)*(index==3))

summary(a$sw_income)

## estimate overall association between x and y:
## 

# marginally standardized 

mod <- glm(hbp ~ race, family = binomial("logit"), data = a)

mu1 <- predict(mod, newdata=transform(a,race=1), type = "response")
mu0 <- predict(mod, newdata=transform(a,race=0), type = "response")

theta_rd <- mean(mu1) - mean(mu0)
theta_rr <- log(mean(mu1) / mean(mu0))

boot_res <- NULL
for(i in 1:500){
  set.seed(i)
  index <- sample(1:nrow(a), nrow(a), replace = T)
  boot_dat <- a[index,]
  
  mod_ <- glm(hbp ~ race, data = boot_dat, family = binomial("logit"))
  
  mu1_ <- predict(mod_, newdata=transform(boot_dat,race=1), type = "response")
  mu0_ <- predict(mod_, newdata=transform(boot_dat,race=0), type = "response")
  
  res_ <- cbind(mean(mu1_) - mean(mu0_), log(mean(mu1_)/mean(mu0_)))
  
  boot_res <- rbind(boot_res, res_)
  
}

res_tabl2_rd <- c(theta_rd, sd(boot_res[,1]))
res_tabl2_rr <- c(theta_rr, sd(boot_res[,2]))

## estimate CDM association between race and hbp:
# determine what the referent level for the mediator is:

# we have a choice: positivity versus interpretation
a %>% 
  group_by(income_cat) %>% 
  count()

## we want to use the (20,22] as the referent category. That way we can 
## interpret our results as the CDM we'd observe if everyone was in the 
## highest income category
## We'll create two indicator (dummy) variables to do this:

a <- a %>% mutate(income_11_17 = as.numeric(income_cat == "[11,17]"),
                  income_17_20 = as.numeric(income_cat == "(17,20]"))

## use LRT to check for race and income interactions

mod1 <- lm(hbp ~ race + income_11_17 + income_17_20 + 
              race*income_11_17 + race*income_17_20 +
              marital + age + school,
            data = a)

mod2 <- lm(hbp ~ race + income_11_17 + income_17_20 + race*income_11_17 +
              marital + age + school, 
            data = a)

mod3 <- lm(hbp ~ race + income_11_17 + income_17_20 + race*income_17_20 +
             marital + age + school, 
           data = a)

lmtest::lrtest(mod1, mod2)
lmtest::lrtest(mod1, mod3)

# neither of these tests suggest important interactions
# however, interaction between race and income_11_17 is more 
# important than for income_17_20
# we will include interaction between ****race and income_11_17****
# for demonstration purposes

## using STRUCTURAL TRANSFORMATION

# start with a regression for estimating effect of mediator on outcome:

struct_trans1_rd <- lm(hbp ~ race + income_11_17 + income_17_20 + 
                        race*income_11_17 +
                         marital + age + school, 
                       data = a)

summary(struct_trans1_rd)

st_coefs_rd <- summary(struct_trans1_rd)$coefficients[c("income_11_17",
                                                        "income_17_20",
                                                        "race:income_11_17"), 1]

struct_trans1_rr <- glm(hbp ~ race + income_11_17 + income_17_20 + 
                         race*income_11_17 +
                         marital + age + school, family=poisson("log"),
                       data = a)

summary(struct_trans1_rr)

st_coefs_rr <- summary(struct_trans1_rr)$coefficients[c("income_11_17",
                                                        "income_17_20",
                                                        "race:income_11_17"), 1]

# create transformed outcomes

a <- a %>% mutate(y_tilde_rd = hbp - income_11_17*st_coefs_rd[1] - income_17_20*st_coefs_rd[2] - 
                    race*income_11_17*st_coefs_rd[3],
                  
                  y_tilde_rr = hbp*exp(- income_11_17*st_coefs_rr[1] - income_17_20*st_coefs_rr[2] - 
                    race*income_11_17*st_coefs_rr[3]))

# estimate CDM

cdm_model_rd <- lm(y_tilde_rd ~ race, data=a)
summary(cdm_model_rd)$coefficients[2,1]

cdm_model_rr <- glm(y_tilde_rr ~ race, family = poisson("log"), data=a)
summary(cdm_model_rr)$coefficients[2,1]

# bootstrap

boot_res_cdm <- NULL
for(i in 1:500){
  set.seed(i)
  index <- sample(1:nrow(a), nrow(a), replace = T)
  boot_dat <- a[index,]
  
  struct_trans1_rd <- lm(hbp ~ race + income_11_17 + income_17_20 + 
                            race*income_11_17 +
                            marital + age + school, 
                          data = boot_dat)
  
  st_coefs_rd <- summary(struct_trans1_rd)$coefficients[c("income_11_17",
                                                          "income_17_20",
                                                          "race:income_11_17"), 1]
  
  struct_trans1_rr <- glm(hbp ~ race + income_11_17 + income_17_20 + 
                            race*income_11_17 +
                            marital + age + school, family=poisson("log"),
                          data = boot_dat)
  
  st_coefs_rr <- summary(struct_trans1_rr)$coefficients[c("income_11_17",
                                                          "income_17_20",
                                                          "race:income_11_17"), 1]
  
  # create transformed outcomes
  
  boot_dat <- boot_dat %>% 
    mutate(y_tilde_rd = hbp - income_11_17*st_coefs_rd[1] - income_17_20*st_coefs_rd[2] - 
            race*income_11_17*st_coefs_rd[3],
                    
           y_tilde_rr = hbp*exp(- income_11_17*st_coefs_rr[1] - income_17_20*st_coefs_rr[2] - 
            race*income_11_17*st_coefs_rr[3]))
  
  # estimate CDM
  
  cdm_model_rd_ <- lm(y_tilde_rd ~ race, data=boot_dat)
  
  cdm_model_rr_ <- glm(y_tilde_rr ~ race, family = poisson("log"), data=boot_dat)
  
  res_cdm <- cbind(summary(cdm_model_rd_)$coefficients[2,1], 
                   summary(cdm_model_rr_)$coefficients[2,1])
  
  boot_res_cdm <- rbind(boot_res_cdm, res_cdm)
  
}

boot_res_cdm

res_tabl4_rd <- c(summary(cdm_model_rd)$coefficients[2,1],sd(boot_res_cdm[,1]))
res_tabl4_rr <- c(summary(cdm_model_rr)$coefficients[2,1],sd(boot_res_cdm[,2]))


## using ip weighting

ipw_model_rd <- lm(hbp ~ race + income_11_17 + income_17_20 + 
                     race*income_11_17, data = a, weights = sw_income)

ipw_model_rr <- glm(hbp ~ race + income_11_17 + income_17_20 + 
                      race*income_11_17, 
                    data = a, 
                    weights = sw_income,
                    family = poisson("log"))

res_tabl5_rd <- coeftest(ipw_model_rd, 
                          vcov = vcovHC(ipw_model_rd, type = "HC3"))[2,1:2]
res_tabl5_rr <- coeftest(ipw_model_rr, 
                         vcov = vcovHC(ipw_model_rr, type = "HC3"))[2,1:2]


## pulling the results together

res_tabl <- data.frame(
  rbind(res_tabl2_rd,
        res_tabl2_rr,
        res_tabl4_rd,
        res_tabl4_rr,
        res_tabl5_rd,
        res_tabl5_rr)
)

row.names(res_tabl) <- c("ATE RD: Marginally Standardized",
                         "ATE RR: Marginally Standardized",
                         "CDM RD: Structural Transformation",
                         "CDM RR: Structural Transformation",
                         "CDM RD: IP Weighted",
                         "CDM RR: IP Weighted")

res_tabl <- res_tabl %>% 
  rownames_to_column(var = "Method")

res_tabl <- res_tabl %>% mutate(LCL = Estimate - 1.96*Std..Error,
                                UCL = Estimate + 1.96*Std..Error)

res_tabl_rd <- res_tabl %>% 
  filter(str_detect(Method,"RD"))

res_tabl_rr <- res_tabl %>% 
  filter(str_detect(Method,"RR")) %>% 
  mutate(Estimate = exp(Estimate),
         LCL = exp(LCL),
         UCL = exp(UCL))

res_tabl <- rbind(res_tabl_rd, 
                  res_tabl_rr)
## export results to spreadsheet

write_csv(res_tabl, here("misc","nhefs_project2_results.csv"))


## some important context: why are the CDMs different from the ATEs?
mean(mu1)
mean(mu0)

cdm_mu1_st <- mean(predict(cdm_model_rd, newdata=transform(a,race=1), type="response"))
cdm_mu0_st <- mean(predict(cdm_model_rd, newdata=transform(a,race=0), type="response"))

cdm_mu1_ipw <- mean(predict(ipw_model_rd, newdata=transform(a,race=1), type="response"))
cdm_mu0_ipw <- mean(predict(ipw_model_rd, newdata=transform(a,race=0), type="response"))

write_csv(
  tibble(
    Model = c("ATE: Marginally Standardized",
              "CDM: Structural Transformation",
              "CDM: IP Weighted"),
    mu1 = c(mean(mu1), cdm_mu1_st, cdm_mu1_ipw),
    mu0 = c(mean(mu0), cdm_mu0_st, cdm_mu0_ipw)
  ), 
  file = here("misc","nhefs_predicted_outcomes_cdm.csv")
)










