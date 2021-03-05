# ExploreData.R
# This R file will analyse the data with visual tools.

## set working directory
setwd("./Rcode")

## reading the data
library(readr)
data = read_table2("../data.txt")
data = na.omit(data) # !!!!!!!!!!!!!!Missing data is omitted, This might need to change!!!!!!!!

## creating descriptive stats
install.packages("pastecs") # you need the pastecs package for the function stat.desc
library(pastecs)
descriptiveData = stat.desc(data) 

## creating visual aid for data analysis
install.packages("ggpubr")
library(ggpubr)
# A separate boxplot per variable
ggboxplot(data, y="weight", xlab="")
ggboxplot(data, y="calhour")
ggboxplot(data, y="calories")
# A combined boxplot for a variables
ggboxplot(data, y=c("weight", "calhour", "calories"), combine=TRUE)
# Scatterplots for the data
pairs(~weight+calhour+calories, data=data, method="pearson",
      lm=TRUE)
lm_weight_calories = lm(weight~calories, data=data)


## estimating correlations
correlation_weight_calhour = cor(data$weight, data$calhour)
correlation_weight_calories = cor(data$weight, data$calories) # Na values make this command fail
correlation_calories_calhour = cor(data$calories, data$calhour) # Na values make this command fail


