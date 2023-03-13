# Disparities Workshop

This repository contains notes, code, data, and exercises for a workshop on analytic methods for health disparities. The workshop will revolve around three projects. Each project will start with a brief lecture on concepts, methods, and context needed to complete the assignment. 

**Requirements for the workshop**: Please bring your laptop to the workshop, and be sure to have working versions of R and RStudio installed before hand. 

As a test of your install, you should be able to run the following code without error:

```
packages <- c("data.table","tidyverse","here")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}
```

The three projects are increasingly complex. Starting with a simple simulated dataset with (mostly) binary variables, to a more complex setting with categorical exposures, mediators, and mixed type confounders. 

The tools and concepts covered in this workshop will include:

  - Data Import, and Wrangling
  - Exploratory Analyses
  - Regression Modeling
  - Counterfactual Disparity Measures
  - Bootstrapping
  - Robust Variance Estimation

All of these will be covered extensively through R code. By the end of this workshop, you should have the ability to deploy code in a reproducible fashion to implement these methods. 

The overall goal of this workshop is to enable you to deploy quantitative methods to answer specific questions pertaining to health disparities.