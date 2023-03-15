packages <- c("data.table","tidyverse","skimr","here", "lmtest", "sandwich")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}

## read in the data

a <- read_csv(here("data","project1_data_analysis.csv"))

head(a)
tail(a)

GGally::ggpairs(a[,c("c","y")])

## start with some basic regression

## outcome model

outcome_model <- lm(y ~ x + l + m + c, data = a)

summary(outcome_model)

## propensity score model exposure

ps_model <- glm(x ~ c, data = a, family = binomial("logit"))

summary(ps_model)

# let's look at the ps overlap

plot_dat <- tibble(
  PS = ps_model$fitted.values,
  Exposure = factor(a$x)
)

ggplot(plot_dat) +
  geom_density(aes(PS, 
                   group = Exposure, 
                   color = Exposure),
               kernel = "epanechnikov") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()

ggsave(here("figures","ps_overlap_exposure.pdf"))

# construct exposure weights

a <- a %>%  mutate(sw_x = (mean(x)/ps_model$fitted.values)*x + 
                     ((1 - mean(x))/(1 - ps_model$fitted.values))*(1 - x))

summary(a$sw_x)

## propensity score model mediator

ps_model_m <- glm(m ~ l + x + c, data = a, family = binomial("logit"))

summary(ps_model_m)

# let's look at the ps overlap

plot_dat <- tibble(
  PS = ps_model_m$fitted.values,
  Mediator = factor(a$m)
)

ggplot(plot_dat) +
  geom_density(aes(PS, 
                   group = Mediator, 
                   color = Mediator),
               kernel = "epanechnikov") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()

ggsave(here("figures","ps_overlap_mediator.pdf"))

# construct mediator weights

a <- a %>%  mutate(sw_m = (mean(m)/ps_model_m$fitted.values)*m + 
                     ((1 - mean(m))/(1 - ps_model_m$fitted.values))*(1 - m))

summary(a$sw_m)

## estimate overall association between x and y:
## 

# crude
res_tabl1a <- summary(lm(y ~ x, data = a))$coefficients[2, 1:2]

# conditionally adjusted
res_tabl1b <- summary(lm(y ~ x + c, data = a))$coefficients[2, 1:2]

# marginally standardized

mod <- lm(y ~ x + c, data = a)

mu1 <- predict(mod, newdata=transform(a,x=1), type = "response")
mu0 <- predict(mod, newdata=transform(a,x=0), type = "response")

theta <- mean(mu1) - mean(mu0)

boot_res <- NULL
for(i in 1:500){
  set.seed(i)
  index <- sample(1:nrow(a), nrow(a), replace = T)
  boot_dat <- a[index,]
  
  mod_ <- lm(y ~ x + c, data = boot_dat)
  
  mu1_ <- predict(mod_, newdata=transform(boot_dat,x=1), type = "response")
  mu0_ <- predict(mod_, newdata=transform(boot_dat,x=0), type = "response")
  
  boot_res <- rbind(boot_res, 
                    mean(mu1_) - mean(mu0_))
  
}

res_tabl2 <- c(theta, sd(boot_res))

# ip weighting: nb, weights were estimated above

mod_ipw <- lm(y ~ x, data = a, weights=sw_x)

res_tabl3 <- coeftest(mod_ipw,
                      vcov = vcovHC(mod_ipw, type = "HC3"))[2,1:2]

## estimate CDM association between x and y:

# determine what the referent level for the mediator is:

a %>% 
  group_by(m) %>% 
  count()

## using structural transformation

# start with a regression for estimating effect of mediator on outcome:

struct_trans1 <- lm(y ~ x + l + m + c + x*m, data = a)

st_coefs <- summary(struct_trans1)$coefficients[c("m","x:m"), 1]

# create transformed outcome

a <- a %>% mutate(y_tilde = y - m*st_coefs[1] - x*m*st_coefs[2])

# estimate CDM

cdm_model <- lm(y_tilde ~ x + c, data=a)

summary(cdm_model)

# bootstrap

boot_res_cdm <- NULL
for(i in 1:500){
  set.seed(i)
  index <- sample(1:nrow(a), nrow(a), replace = T)
  boot_dat <- a[index,]
  
  struct_trans1_ <- lm(y ~ x + l + m + c + x*m, data = boot_dat)
  st_coefs_ <- summary(struct_trans1_)$coefficients[c("m","x:m"), 1]
  
  boot_dat <- boot_dat %>% mutate(y_tilde = y - m*st_coefs_[1] - x*m*st_coefs_[2])
  mod_ <- lm(y_tilde ~ x + c, data = boot_dat)
  
  cdm_est <- summary(mod_)$coefficients[2,1]
  
  boot_res_cdm <- rbind(boot_res_cdm, cdm_est)
  
}

boot_res_cdm

res_tabl4 <- c(summary(cdm_model)$coefficients[2,1],sd(boot_res_cdm))


## using ip weighting

head(a)

ipw_model <- lm(y ~ x + m + x*m, data = a, weights = sw_x*sw_m)

res_tabl5 <- coeftest(ipw_model, 
                      vcov = vcovHC(ipw_model, type = "HC3"))[2,1:2]


## pulling the results together

res_tabl <- data.frame(
  rbind(res_tabl1a,
        res_tabl1b,
        res_tabl2,
        res_tabl3,
        res_tabl4,
        res_tabl5)
)

row.names(res_tabl) <- c("ATE: Crude", 
                         "ATE: Conditionally Adjusted",
                         "ATE: Marginally Standardized",
                         "ATE: IP Weighted",
                         "CDM: Structural Transformation",
                         "CDM: IP Weighted") 

res_tabl <- res_tabl %>% 
  rownames_to_column(var = "Method")

res_tabl <- res_tabl %>% mutate(LCL = Estimate - 1.96*Std..Error,
                                UCL = Estimate + 1.96*Std..Error)


## export results to spreadsheet

write_csv(res_tabl, here("misc","project1_results.csv"))


## some important context: why are the CDMs different from the ATEs?
mean(mu1)
mean(mu0)

ate_mu1_ipw <- mean(predict(mod_ipw, newdata=transform(a,x=1), type="response"))
ate_mu0_ipw <- mean(predict(mod_ipw, newdata=transform(a,x=0), type="response"))

cdm_mu1_st <- mean(predict(cdm_model, newdata=transform(a,x=1), type="response"))
cdm_mu0_st <- mean(predict(cdm_model, newdata=transform(a,x=0), type="response"))

cdm_mu1_ipw <- mean(predict(ipw_model, newdata=transform(a,x=1), type="response"))
cdm_mu0_ipw <- mean(predict(ipw_model, newdata=transform(a,x=0), type="response"))

write_csv(
  tibble(
    Model = c("ATE: Marginally Standardized",
              "ATE: IP Weighted",
              "CDM: Structural Transformation",
              "CDM: IP Weighted"),
    mu1 = c(mean(mu1), ate_mu1_ipw, cdm_mu1_st, cdm_mu1_ipw),
    mu0 = c(mean(mu0), ate_mu0_ipw, cdm_mu0_st, cdm_mu0_ipw)
  ), 
  file = here("misc","predicted_outcomes_cdm.csv")
)










