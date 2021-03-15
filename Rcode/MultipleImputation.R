# MultipleImputations.R

## setting the working directory
install.packages('rstudioapi')
library("rstudioapi") 
# set working directory
setwd(dirname(getActiveDocumentContext()$path))
## read data
dataframe <- read.table('../muscle-incomplete.txt', header = TRUE)

## Multiple imputation analysis
# packages needed for MI analysis
install.packages('mice')
library(mice)

# Multiple imputation analysis
numberOfMIRounds = 1000
imp = mice(dataframe, m=numberOfMIRounds) # The 'mice' function does a MI analysis with m = number of rounds of MI

# check (visually) if imputed values are not 'weird'
imp$imp$calories
# Each column are the imputed values for 1 round of IM. The rows are the imputed values for
# the missing values in the data set. As you can see, generally speaking the values are looking okay.

# Check with a plot if the imputed values are not out of the ordinary.
#!!!!!!! This is taken directly out of the slides and I do not understand it properly,
# we should take care when using this in the assignment. Copying things is not taken lightly at KULeuven
col = rep(c("blue", "red")[1+as.numeric(is.na(imp$data$calories))], numberOfMIRounds+1)
stripplot(calories~.imp, data=completeDataframe, jit=TRUE,  col=col, 
          pch=20, cex=1.4, xlab="impuation number")
# The plot shows that the red (imputed) values are approximately randomly distributed between$
# the observed values.

# Analysing the imputed data
# The function 'with' evaluates an R expression in an environment constructed from the given data.
# The glm function fits a generalized linear model.
# building a model which correlates weight to calories
fit_weight_calories = with(data=imp, exp=glm(calories~weight))
estimates_weight_calories = pool(fit_weight_calories)
summary(estimates_weight_calories)
# ....

# building a model which correlates calhour to calories
fit_calhour_calories = with(data=imp, exp=glm(calories~calhour))
estimates_calhour_calories = pool(fit_calhour_calories)
summary(estimates_calhour_calories)
# ....

# building a model which correlates weight and calhour to calories
fit_weight_calhour_calories = with(data=imp, exp=glm(calories~calhour+weight))
estimates_weight_calhour_calories = pool(fit_weight_calhour_calories)
summary(estimates_weight_calhour_calories)
# ....


# KLAAS ALTERNATIVELY- START

# Multiple imputation analysis

# Standard: pnn
imp <- mice(dataframe, m=100)
imp
# other option: bayesian linear
imp <- mice(dataframe, meth = c("", "", "norm"), m=100)
imp
# other option: bayesian non-linear
imp <- mice(dataframe, meth = c("", "", "norm.nob"), m=100)
imp
# play around with them and see what final model including imputations gives --> maybe overkill?


## Imputed values for calories 
## The columns contain the multiple imputations.
imp$imp$calories[1:10,1:5]

## The complete data combine observed and imputed data.
complete(imp,1)[1:24,]

## Plot the imputated values --> looks ok, same distribution as observed values
com <- complete(imp, "long", inc=T)
col <- rep(c("green","black")[1+as.numeric(is.na(imp$data$calories))],101)
stripplot(calories~.imp, data=com, jit=TRUE, fac=0.8, col=col, pch=20, cex=1.4, xlab="Imputation number")

## Analyzing the imputed data sets

fit <- with(data=imp, exp=lm(calories ~ calhour + weight ))

## Creating a data set with the results of all the analysis

MI.matrix<-matrix(0,100,3)
for(k in 1:100) MI.matrix[k,]<-coefficients(fit$analyses[[k]])
MI.results=data.frame(Intercept=MI.matrix[,1], calhour=MI.matrix[,2],weight=MI.matrix[,3])
MI.results[1:10,]
## parameters for calhour stable, for weight more variation but in same line


est <- pool(fit)
summary(est)
## compare this with complete case and IPW

# KLAAS ALTERNATIVELY- STOP







