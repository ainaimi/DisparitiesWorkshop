packages <- c("data.table","tidyverse","skimr","here")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}

# import and explore the data:

a <- read_csv(here("data","project1_data_raw.csv"))

head(a)

skimr::skim(a)

GGally::ggpairs(a[,c("c","y")])

ggplot(a) +
  geom_histogram(aes(x=c)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()

ggplot(a) +
  geom_histogram(aes(x=y)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()

ggplot(a) +
  geom_point(aes(x=c, y=y)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()

## let's log-transform the confounder:
## in this case, re-write the original c 

b <- a %>% mutate(c = log(c))

ggplot() +
  geom_point(aes(x=a$c, y=b$c)) +
  scale_x_continuous(expand = c(0,0), limits = c(0,40)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()

## export the data to the data folder

write_csv(b, here("data","project1_data_analysis.csv"))