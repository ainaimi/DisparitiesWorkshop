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

a <- read_csv(here("data","analytic_numom.csv")) %>% 
  mutate(education = factor(education, levels = c("[Less than HS grad]",
                                                  "[HS grad or GED]",
                                                  "[Assoc/Tech degree]",
                                                  "[Some college]",
                                                  "[Completed college]",
                                                  "[Degree work beyond college]")))
head(a)
tail(a)

## start with some basic regression
## outcome model

names(a)

a %>% count(v1_income)
a %>% count(education)

outcome_model_add <- lm(pree_acog ~ education + married + I(v1_income>9) + momage + bmiprepreg +
                          hei2015_total + pa_totmetwk_v1 + prediab1 + prehtn1, data = a)
summary(outcome_model_add)

## propensity score model exposure (education adjusted for race)

ps_model_m <- vglm(education ~ momrace4 + dadrace4, 
                   data = a, family = multinomial)
summary(ps_model_m)

# let's look at the fitted values (propensity score) from this model
head(ps_model_m@fitted.values)

## create an index to pick off the right PS column
a <- a %>% mutate(index = as.numeric(education == "[Less than HS grad]")*1 +
                          as.numeric(education == "[HS grad or GED]")*2 +
                          as.numeric(education == "[Assoc/Tech degree]")*3 +
                          as.numeric(education == "[Some college]")*4 +
                          as.numeric(education == "[Completed college]")*5 +
                          as.numeric(education == "[Degree work beyond college]")*6)

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
  Education = factor(a$education)
)

ggplot(plot_dat) +
  geom_density(aes(PS, 
                   group = Education, 
                   color = Education),
               kernel = "epanechnikov",
               bw = .1) +
  scale_x_continuous(expand = c(0,0), limits=c(0,1)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()

ggsave(here("figures","ps_overlap_education.pdf"))

# construct exposure weights
a <- a %>%  mutate(sw_education = (mean(index==1)/propensity_score)*(index==1) +
                                  (mean(index==2)/propensity_score)*(index==2) +
                                  (mean(index==3)/propensity_score)*(index==3) +
                                  (mean(index==4)/propensity_score)*(index==4) +
                                  (mean(index==5)/propensity_score)*(index==5) +
                                  (mean(index==6)/propensity_score)*(index==6))

summary(a$sw_education)

## propensity score model mediator (cigs)
ps_model_m <- glm(as.numeric(cigsprepreg>0) ~ married + I(v1_income>9) + ns(momage, df = 3) + ns(bmiprepreg, df = 3) +
                    ns(hei2015_total, df = 3) + ns(pa_totmetwk_v1, df = 3) + prediab1 + prehtn1, 
                  data = a, family = binomial("logit"))
summary(ps_model_m)

# let's look at the fitted values (propensity score) from this model
head(ps_model_m$fitted.values)

## create PS column in data
a <- a %>% mutate(propensity_score_cigs = ps_model_m$fitted.values)

# let's look at the ps overlap

plot_dat <- tibble(
  PS = a$propensity_score_cigs,
  `Prepregnancy Smoker` = factor(as.numeric(a$cigsprepreg>0))
)

ggplot(plot_dat) +
  geom_density(aes(PS, 
                   group = `Prepregnancy Smoker`, 
                   color = `Prepregnancy Smoker`),
               kernel = "epanechnikov",
               bw = .1) +
  scale_x_continuous(expand = c(0,0), limits=c(0,1)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()

ggsave(here("figures","ps_overlap_cigs.pdf"))

# construct exposure weights
a <- a %>%  mutate(sw_cigs = (mean(as.numeric(a$cigsprepreg>0))/propensity_score_cigs)*as.numeric(a$cigsprepreg>0) + 
                     (mean(1 - as.numeric(a$cigsprepreg>0))/(1 - propensity_score_cigs))*(1 - as.numeric(a$cigsprepreg>0)))

summary(a$sw_cigs)


## estimate overall association between x and y:
## 

# marginally standardized 
mod <- glm(pree_acog ~ education + momrace4 + dadrace4, family = binomial("logit"), data = a)

####
#### NB: we are obtaining treatment specific means here. We can pick whatever contrast we would like
#### compare "completed college" to "less than HS grad"
mu1 <- predict(mod, newdata=transform(a,education  = "[Completed college]"), type = "response")
mu0 <- predict(mod, newdata=transform(a,education  = "[Less than HS grad]"), type = "response")

theta_rd <- mean(mu1) - mean(mu0)
theta_rr <- log(mean(mu1) / mean(mu0))

boot_res <- NULL
for(i in 1:500){
  set.seed(i)
  index <- sample(1:nrow(a), nrow(a), replace = T)
  boot_dat <- a[index,]
  
  mod_ <- glm(pree_acog ~ education + momrace4 + dadrace4, family = binomial("logit"), data = boot_dat)
  
  mu1_ <- predict(mod_, newdata=transform(boot_dat,education  = "[Completed college]"), type = "response")
  mu0_ <- predict(mod_, newdata=transform(boot_dat,education  = "[Less than HS grad]"), type = "response")
  
  res_ <- cbind(mean(mu1_) - mean(mu0_), log(mean(mu1_)/mean(mu0_)))
  
  boot_res <- rbind(boot_res, res_)
  
}

res_tabl1_rd <- c(theta_rd, sd(boot_res[,1]))
res_tabl1_rr <- c(theta_rr, sd(boot_res[,2]))

# IP weighted
mod_ip_rd <- glm(pree_acog ~ education, 
                 family = quasibinomial("identity"), 
                 weights = sw_education, 
                 data = a)

res_tabl2_rd <- coeftest(mod_ip_rd,
                         vcov = vcovHC(mod_ip_rd, type = "HC3"))[5,1:2]

mod_ip_rr <- glm(pree_acog ~ education, 
                 family = quasibinomial("log"), 
                 weights = sw_education, 
                 data = a)

res_tabl2_rr <- coeftest(mod_ip_rr,
                         vcov = vcovHC(mod_ip_rr, type = "HC3"))[5,1:2]

## estimate CDM association between education and preeclampsia:
## the referent for the mediator is no prepregnancy smoking
## we want to use 0 as the referent category. That way we can 
## interpret our results as the CDM we'd observe if no pregnancy women smoked 
## prior to pregnancy

## use LRT to check for education and cigs interactions

mod1 <- lm(pree_acog ~ education + as.numeric(cigsprepreg>0) +
             education*as.numeric(cigsprepreg>0),
            data = a)

mod2 <- lm(pree_acog ~ education + as.numeric(cigsprepreg>0), 
            data = a)

lmtest::lrtest(mod1, mod2)

# this test suggests important interactions

## using STRUCTURAL TRANSFORMATION

# start with a regression for estimating effect of mediator on outcome:

struct_trans1_rd <- lm(pree_acog ~ education + I(cigsprepreg>0) + 
                         education*I(cigsprepreg>0) + married + 
                         I(v1_income>9) + ns(momage, df = 3) + 
                         ns(bmiprepreg, df = 3) + ns(hei2015_total, df = 3) + 
                         ns(pa_totmetwk_v1, df = 3) + prediab1 + prehtn1, 
                       data = a)

summary(struct_trans1_rd)

st_coefs_rd <- summary(struct_trans1_rd)$coefficients[c("I(cigsprepreg > 0)TRUE",
                                                        "education[HS grad or GED]:I(cigsprepreg > 0)TRUE",              
                                                        "education[Assoc/Tech degree]:I(cigsprepreg > 0)TRUE",
                                                        "education[Some college]:I(cigsprepreg > 0)TRUE",        
                                                        "education[Completed college]:I(cigsprepreg > 0)TRUE",
                                                        "education[Degree work beyond college]:I(cigsprepreg > 0)TRUE"), 1]

struct_trans1_rr <- glm(pree_acog ~ education + I(cigsprepreg>0) + 
                          education*I(cigsprepreg>0) + married + 
                          I(v1_income>9) + ns(momage, df = 3) + 
                          ns(bmiprepreg, df = 3) + ns(hei2015_total, df = 3) + 
                          ns(pa_totmetwk_v1, df = 3) + prediab1 + prehtn1, 
                        family=poisson("log"),
                       data = a)

summary(struct_trans1_rr)

st_coefs_rr <- summary(struct_trans1_rr)$coefficients[c("I(cigsprepreg > 0)TRUE",
                                                        "education[HS grad or GED]:I(cigsprepreg > 0)TRUE",              
                                                        "education[Assoc/Tech degree]:I(cigsprepreg > 0)TRUE",
                                                        "education[Some college]:I(cigsprepreg > 0)TRUE",        
                                                        "education[Completed college]:I(cigsprepreg > 0)TRUE",
                                                        "education[Degree work beyond college]:I(cigsprepreg > 0)TRUE"), 1]



# create transformed outcomes

a <- a %>% mutate(y_tilde_rd = pree_acog - I(cigsprepreg>0)*st_coefs_rd[1] - 
                    I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rd[2] -
                    I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rd[3] - 
                    I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rd[4] -
                    I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rd[5] -
                    I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rd[6],
                  
                  y_tilde_rr = pree_acog*exp(-I(cigsprepreg>0)*st_coefs_rr[1] - 
                                               I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rr[2] -
                                               I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rr[3] - 
                                               I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rr[4] -
                                               I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rr[5] -
                                               I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rr[6]))

# estimate CDM

cdm_model_rd <- lm(y_tilde_rd ~ education, data=a)
summary(cdm_model_rd)$coefficients[5,1]

cdm_model_rr <- glm(y_tilde_rr ~ education, family = quasipoisson("log"), data=a)
summary(cdm_model_rr)$coefficients[5,1]

# bootstrap

boot_res_cdm <- NULL
for(i in 1:500){
  set.seed(i)
  index <- sample(1:nrow(a), nrow(a), replace = T)
  boot_dat <- a[index,]
  
  struct_trans1_rd_ <- lm(pree_acog ~ education + I(cigsprepreg>0) + 
                           education*I(cigsprepreg>0) + married + 
                           I(v1_income>9) + ns(momage, df = 3) + 
                           ns(bmiprepreg, df = 3) + ns(hei2015_total, df = 3) + 
                           ns(pa_totmetwk_v1, df = 3) + prediab1 + prehtn1, 
                         data = boot_dat)
  
  st_coefs_rd_ <- summary(struct_trans1_rd_)$coefficients[c("I(cigsprepreg > 0)TRUE",
                                                            "education[HS grad or GED]:I(cigsprepreg > 0)TRUE",              
                                                            "education[Assoc/Tech degree]:I(cigsprepreg > 0)TRUE",
                                                            "education[Some college]:I(cigsprepreg > 0)TRUE",        
                                                            "education[Completed college]:I(cigsprepreg > 0)TRUE",
                                                            "education[Degree work beyond college]:I(cigsprepreg > 0)TRUE"), 1]
  
  struct_trans1_rr_ <- glm(pree_acog ~ education + I(cigsprepreg>0) + 
                            education*I(cigsprepreg>0) + married + 
                            I(v1_income>9) + ns(momage, df = 3) + 
                            ns(bmiprepreg, df = 3) + ns(hei2015_total, df = 3) + 
                            ns(pa_totmetwk_v1, df = 3) + prediab1 + prehtn1, 
                          family=poisson("log"),
                          data = boot_dat)
  
  st_coefs_rr_ <- summary(struct_trans1_rr_)$coefficients[c("I(cigsprepreg > 0)TRUE",
                                                            "education[HS grad or GED]:I(cigsprepreg > 0)TRUE",              
                                                            "education[Assoc/Tech degree]:I(cigsprepreg > 0)TRUE",
                                                            "education[Some college]:I(cigsprepreg > 0)TRUE",        
                                                            "education[Completed college]:I(cigsprepreg > 0)TRUE",
                                                            "education[Degree work beyond college]:I(cigsprepreg > 0)TRUE"), 1]
  
  # create transformed outcomes
  
  boot_dat <- boot_dat %>% mutate(y_tilde_rd = pree_acog - I(cigsprepreg>0)*st_coefs_rd_[1] - 
                                    I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rd_[2] -
                                    I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rd_[3] - 
                                    I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rd_[4] -
                                    I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rd_[5] -
                                    I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rd_[6],
                                  
                                  y_tilde_rr = pree_acog*exp(-I(cigsprepreg>0)*st_coefs_rr_[1] - 
                                                               I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rr_[2] -
                                                               I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rr_[3] - 
                                                               I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rr_[4] -
                                                               I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rr_[5] -
                                                               I(education == "[Completed college]")*I(cigsprepreg>0)*st_coefs_rr_[6]))
  
  # estimate CDM
  
  cdm_model_rd_ <- lm(y_tilde_rd ~ education, data=boot_dat)
  rd_boot_ <- summary(cdm_model_rd_)$coefficients[5,1]
  
  cdm_model_rr_ <- glm(y_tilde_rr ~ education, family = quasipoisson("log"), data=boot_dat)
  rr_boot_ <- summary(cdm_model_rr_)$coefficients[5,1]
  
  res_cdm <- cbind(rd_boot_, rr_boot_)
  
  boot_res_cdm <- rbind(boot_res_cdm, res_cdm)
  
}

boot_res_cdm

res_tabl4_rd <- c(summary(cdm_model_rd)$coefficients[5,1],sd(boot_res_cdm[,1]))
res_tabl4_rr <- c(summary(cdm_model_rr)$coefficients[5,1],sd(boot_res_cdm[,2]))

## using ip weighting

ipw_model_rd <- lm(pree_acog ~ education + I(cigsprepreg>0) + 
                     education*I(cigsprepreg>0), data = a, weights = sw_education*sw_cigs)

ipw_model_rr <- glm(pree_acog ~ education + I(cigsprepreg>0) + 
                      education*I(cigsprepreg>0), data = a, weights = sw_education*sw_cigs, 
                    family = poisson("log"))

res_tabl5_rd <- coeftest(ipw_model_rd, 
                          vcov = vcovHC(ipw_model_rd, type = "HC3"))[5,1:2]
res_tabl5_rr <- coeftest(ipw_model_rr, 
                         vcov = vcovHC(ipw_model_rr, type = "HC3"))[5,1:2]


## pulling the results together

res_tabl <- data.frame(
  rbind(res_tabl1_rd,
        res_tabl1_rr,
        res_tabl2_rd,
        res_tabl2_rr,
        res_tabl4_rd,
        res_tabl4_rr,
        res_tabl5_rd,
        res_tabl5_rr)
)

row.names(res_tabl) <- c("ATE RD: Marginally Standardized",
                         "ATE RR: Marginally Standardized",
                         "ATE RD: IP Weighted",
                         "ATE RR: IP Weighted",
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

res_tabl
## export results to spreadsheet

write_csv(res_tabl, here("misc","numom_project3_results.csv"))


## some important context: why are the CDMs different from the ATEs?
mean(mu1)
mean(mu0)

cdm_mu1_st <- mean(predict(cdm_model_rd, newdata=transform(a,education  = "[Completed college]"), type="response"))
cdm_mu0_st <- mean(predict(cdm_model_rd, newdata=transform(a,education  = "[Less than HS grad]"), type="response"))

cdm_mu1_ipw <- mean(predict(ipw_model_rd, newdata=transform(a,education  = "[Completed college]"), type="response"))
cdm_mu0_ipw <- mean(predict(ipw_model_rd, newdata=transform(a,education  = "[Less than HS grad]"), type="response"))

write_csv(
  tibble(
    Model = c("ATE: Marginally Standardized",
              "CDM: Structural Transformation",
              "CDM: IP Weighted"),
    mu1 = c(mean(mu1), cdm_mu1_st, cdm_mu1_ipw),
    mu0 = c(mean(mu0), cdm_mu0_st, cdm_mu0_ipw)
  ), 
  file = here("misc","numom_predicted_outcomes_cdm.csv")
)










