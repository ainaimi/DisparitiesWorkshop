packages <- c("data.table","tidyverse","skimr","here","splines")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

expit <- function(x){1/(1+exp(-x))}

# import and format the data

file_loc <- url("https://cdn1.sph.harvard.edu/wp-content/uploads/sites/1268/1268/20/nhefs.csv")

nhefs <- read_csv(file_loc) %>% 
  select(race, income, marital, age, school, birthplace, hightax82, dbp, sbp) %>% 
  mutate(hbp = if_else(sbp>=140&dbp>=90,
                       1,
                       0)) %>% 
  na.omit(.)

# explore data
nhefs

skimr::skim(nhefs)

# look at the relation between race and hbp
summary(glm(hbp ~ race, data = nhefs, family = binomial("logit")))$coefficients

# look at the relation between income and hbp

## create cutpoints for income
bk <- c(11,14,18,22)

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

ggplot(newdat) +
  geom_line(aes(y = risk1, x = income), color="red") +
  geom_line(aes(y = risk2, x = income), color="blue") +
  geom_point(aes(y = risk3, x = income), color="black")

# look at the relation between race and income

summary(lm(income ~ race, data = nhefs))$coefficients

