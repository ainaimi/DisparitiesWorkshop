packages <- c("data.table","tidyverse","skimr","here")

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

# data gen for SEM example

n <- 2000
c <- rnorm(n)
x <- rbinom(n,1,expit(-2 + log(1.5)*c))
l <- rbinom(n,1,expit(-2 + log(2.5)*x))
m <- rbinom(n,1,expit(-2 + log(1.5)*c + log(2.5)*x + log(2.5)*l))
y <- 120 + 1.5*c + 1.5*x + 2.5*l + 1.5*m + rnorm(n)
c <- exp(c)
a <- tibble(c,x,l,m,y)

a

summary(a)

write_csv(a, here("project1", "project1_data_raw.csv"))