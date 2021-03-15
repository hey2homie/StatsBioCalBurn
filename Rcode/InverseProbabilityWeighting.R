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

#### Klaas Alternatively - start
dataframe$r<-as.numeric(!is.na(dataframe$calories))
head(dataframe,30)
#### Klaas Alternatively - stop




# Calculating the weights (modeling r in function of calhour)
missing_model = glm(data=dataframe, family=binomial, missing~calhour)
dataframe$w = 1/fitted(missing_model)

# Calculating the model for calories in function of calhour
fit_calhour_calories = glm(calories~calhour, data=dataframe, weights=dataframe$w)
summary(fit_calhour_calories)



#### Klaas Alternatively - start
# Calculating the weights (modeling r in function of calhour AND weight - to be seen if necessary)
dataframe.ipw<-glm(r ~  calhour + weight , data=dataframe,family=binomial)
summary(dataframe.ipw)
# Note that the logistic regression of being a complete observation does not result in significant variables for calhour and weight, though the missing cases appear all on the lower calhour so significant calhour 
# was expected - doing something wrong here?
## Calculating the weights: Inverse Probabilities
dataframe$w<-1/fitted(dataframe.ipw)
head(dataframe,30)
# NOTE: the weights are all bigger for the NA cases, but in the IPW, only observations complete are taken into account for model. The difference with complete case is just adding weights. 
# NOTE: So conclusion: since missing are all 100% missing in the lover calhour, weights in the complete observations are almost equal, so the difference between IPW and complete case analysis is minimal --> porbably multiple imputation is to be preferred since it takes into account alll observations and the number of observations is already limited

# Calculating the model for calories in function of calhour AND weight
fit_calhour_calories = glm(calories~calhour = weight, data=dataframe, weights=dataframe$w)
summary(fit_calhour_calories)

#### Alternatively
#### Klaas Alternatively - stop



