packages <- c("data.table","tidyverse","skimr","here", 
              "splines", "ggExtra", "gridExtra", "VGAM",
              "haven", "caret")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}

# import and explore the data:
a <- read_csv(here("data","simulated_numom.csv")) %>% 
  mutate(education = ordered(education, levels = c("[Less than HS grad]",
                                                   "[HS grad or GED]",
                                                   "[Assoc/Tech degree]",
                                                   "[Some college]",
                                                   "[Completed college]",
                                                   "[Degree work beyond college]")),
         pree_acog = factor(pree_acog),
         married = factor(married))

skimr::skim(a)

a %>% count(pree_acog)
a %>% count(v1_income)
a %>% count(education)
a %>% count(momrace4)
a %>% count(dadrace4)
a %>% count(as.numeric(cigsprepreg>0))
a %>% count(as.numeric(cigsduring>0))

summary(lm(as.numeric(cigsprepreg>0) ~ factor(education), data = a))

summary(lm(pree_acog ~ as.numeric(cigsprepreg>1) , data = a))
summary(lm(pree_acog ~ as.numeric(cigsduring>1) , data = a))

summary(lm(pree_acog ~ factor(education), data = a))

hist(a$cigsprepreg)
summary(a$cigsprepreg)
hist(a$cigsduring)
summary(a$cigsduring)

write_csv(a, here("data", "analytic_numom.csv"))