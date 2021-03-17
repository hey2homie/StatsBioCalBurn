# MissingData.R
# This R file will analyse the missing data with visual tools.

## setting the working directory
install.packages('rstudioapi')
library("rstudioapi") 
## set working directory
setwd(dirname(getActiveDocumentContext()$path))
## read data
dataframe <- read.table('../muscle-incomplete.txt', header = TRUE)

## packages needed for missing data analysis
install.packages('VIM')
install.packages("finalfit")
library(VIM)
library(finalfit)

## packages needed for dataframe management
install.packages('tidyverse')
library(tidyverse)

## Missing data analysis
missingData = aggr(dataframe, numbers=TRUE, prop=FALSE) # for the numbers and prop arguments type help(aggr) in the console
missingData
# The missingData variable contains a figure which tells you that
# we miss 8 values for calories. And that we have a "complete" data set for
# 16 entries in the dataframe.

missingPlot_weight =
  select(dataframe, weight, calories) %>% # selecting the columns weight and calories
  arrange(weight) %>% # sorting the observations by the values of the covariate weight
  missing_plot() # This function creates the missing plot
missingPlot_weight # sorting the observations by the values of the covariate calhour

missingPlot_calhour =
  select(dataframe, calhour, calories) %>% # selecting the columns calhour and calories
  arrange(calhour) %>% # This function creates the missing plot
  missing_plot()
missingPlot_calhour
# The missing plot shows the distrubution of the missing values
# It appears that the missing values in calories are NOT correlated with 
# the covariate weight
# However, it is very clear that for the covariate calhour the missing 
# values are more abundant for 'low' calhour values.
# Because the missing values depend on a covariate for which we do have data
# we are dealing with a 'missing at random' case (MAR)
# Therefore a complete case analysis might yield baised results.
# The multiple imputation and inverse probability weighting methods will
# yield better results in this case.
