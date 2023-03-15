packages <- c("data.table","tidyverse","skimr","here", 
              "splines", "ggExtra", "gridExtra", "VGAM")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}

# import and explore the data:

file_loc <- url("https://cdn1.sph.harvard.edu/wp-content/uploads/sites/1268/1268/20/nhefs.csv")

nhefs <- read_csv(file_loc) %>% 
  write_csv(., here("data","nhefs_raw_data.csv")) %>% 
  select(race, income, marital, age, school, dbp, sbp) %>% 
  mutate(hbp = factor(if_else(sbp>=140&dbp>=90,
                       1,
                       0)),
         race = factor(race),
         marital = factor(as.numeric(marital>2))) %>% 
  select(-sbp,-dbp) %>% 
  na.omit(.)

# explore data
nhefs

skimr::skim(nhefs)

GGally::ggpairs(nhefs)

# look at the relation between race and hbp
summary(glm(hbp ~ race, data = nhefs, family = binomial("logit")))$coefficients

# look at the relation between income and hbp

## create cutpoints for income
bk <- c(11,17,20,22)

nhefs$income_cat <- factor(
  cut(
    nhefs$income, 
    breaks = bk,
    include.lowest = T
  )
)

mod_income1 <- glm(hbp ~ income, data = nhefs, family = binomial("logit"))
mod_income2 <- glm(hbp ~ ns(income, df=2), data = nhefs, family = binomial("logit"))
mod_income3 <- glm(hbp ~ income_cat, data = nhefs, family = binomial("logit"))

newdat <- data.frame(income = seq(11,22,1),
                     income_cat = factor(
                       cut(
                         seq(11,22,1), 
                         breaks = bk,
                         include.lowest = T
                       )
                     ))

newdat$risk1 <- predict(mod_income1,newdata = newdat,type = "response")
newdat$risk2 <- predict(mod_income2,newdata = newdat,type = "response")
newdat$risk3 <- predict(mod_income3,newdata = newdat,type = "response")

p1 <- ggplot() +
  geom_line(data = newdat, aes(y = risk1, x = income), color="red") +
  geom_line(data = newdat, aes(y = risk2, x = income), color="blue") +
  geom_point(data = newdat, aes(y = risk3, x = income), color="black") +
  theme_classic() + ylab("High Blood Pressure Risk")

p2 <- ggplot() +
  geom_histogram(data = nhefs, aes(x = income), color="black") +
  theme_classic() + ylab("Count")

grid.arrange(p1,p2,ncol=1)

# look at the relation between race and income

summary(lm(income ~ race, data = nhefs))$coefficients

summary(vglm(income_cat ~ race, data = nhefs, family = multinomial))


# export analysis dataset

write_csv(nhefs,here("data","nhefs_analytic_data.csv"))