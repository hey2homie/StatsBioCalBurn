# InverseProbabilityWeighting.R

## setting the working directory
install.packages('rstudioapi')
library("rstudioapi") 
# set working directory
setwd(dirname(getActiveDocumentContext()$path))
## read data
dataframe <- read.table('../muscle-incomplete.txt', header = TRUE)

## Installing packages needed for dataframe manipulation
install.packages('tidyverse')
library(tidyverse)

## Inverse probabilty weighting
# Creating the missing variable (missing=0 if calories is missing)
dataframe = mutate(dataframe, missing = as.numeric(!is.na(dataframe$calories)))

# Calculating the weights (modeling r in function of calhour)
missing_model = glm(data=dataframe, family=binomial, missing~calhour)
dataframe$w = 1/fitted(missing_model)
#!!!! it appears I got some weird values for the weight? can somebody help :o

# Calculating the model for calories in function of calhour
fit_calhour_calories = glm(calories~calhour, data=dataframe, weights=dataframe$w)
summary(fit_calhour_calories)
