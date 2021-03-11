# CompleteCase.R

## setting the working directory
install.packages('rstudioapi')
library("rstudioapi") 
## set working directory
setwd(dirname(getActiveDocumentContext()$path))
## read data
dataframe <- read.table('../muscle-incomplete.txt', header = TRUE)