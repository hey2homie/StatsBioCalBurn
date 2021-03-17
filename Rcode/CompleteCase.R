## CompleteCase.R
## setting the working directory
install.packages('rstudioapi')
library("rstudioapi") 
## set working directory
setwd(dirname(getActiveDocumentContext()$path))
## read data
dataframe <- read.table('../muscle-incomplete.txt', header = TRUE)
dataframe <- na.omit(dataframe)		# Omiting NAs to carry out complete case analysis

library(tidyverse)
library(gridExtra)

# Exploring the results of linear regression in case of complete analysis
fit <- lm(calhour ~ calories, data = dataframe)
fit1 <- lm(calhour ~ calories + weight, data = dataframe)

dataframe$predicted <- predict(fit)
dataframe$predicted1 <- predict(fit1)
dataframe$residuals <- residuals(fit)
dataframe$residuals1 <- residuals(fit1)

# Diagnosis plots
model.plot <- function(df, model, pred, number = "") {

	plot <- ggplot(df, aes(x = calories, y = calhour)) +
		geom_point(aes(alpha = abs(model$residuals), color = abs(model$residuals), size = abs(model$residuals))) +	# Plot actual values
		scale_color_continuous(low = 'black', high = 'red') +
		geom_point(aes(y = pred), col = 'steelblue') +	# Plot values predicted by first model
		geom_segment(aes(xend = calories, yend = pred), alpha = 0.5) +
		labs(x = 'Heat Production', y = 'Calories per Hour',
			 title = sprintf('Visualization of linear model%s', number)) +
		# TODO: Cannot add legend
		scale_fill_manual(name = ' ', values = c('steelblue', 'tomato3'), labels = c('Predicted', 'Actual')) +
		guides(alpha = FALSE, color = FALSE, size = FALSE) +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	if(number == '') {
			plot <- plot + geom_line(stat = 'smooth', method = 'lm', alpha = 0.5, se = FALSE, color = 'black')
	}

	return(plot)
}

grid.arrange(model.plot(dataframe, fit, dataframe$predicted),
			 model.plot(dataframe, fit1, dataframe$predicted1, 1), ncol = 2)

model.residuals <- ggplot(dataframe, aes(x = residuals)) +
	geom_histogram(fill = 'steelblue', color = 'black', alpha = 0.9) +
	labs(x = 'Residual Values', y = 'Frequency', title = 'Residuals Destribution Model') +
	theme_classic() +
	theme(plot.title = element_text(hjust = 0.5))

model1.residuals <- ggplot(dataframe, aes(x = residuals1)) +
	geom_histogram(fill = 'steelblue', color = 'black', alpha = 0.9) +
	labs(x = 'Residual Values', y = 'Frequency', title = 'Residuals Destribution Model1') +
	theme_classic() +
	theme(plot.title = element_text(hjust = 0.5))

plotResiduals <- function(model, number = "") {

	plot <- ggplot(model, aes(x = model$residuals)) +
		geom_histogram(fill = 'steelblue', color = 'black', alpha = 0.9) +
		labs(x = 'Residual Values', y = 'Frequency', title = sprintf('Residuals Destribution of Model%s', number)) +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

grid.arrange(plotResiduals(fit), plotResiduals(fit1, 1), ncol = 2)

residualsVsFitted <- function(model, number = "") {

	plot <- ggplot(model, aes(.fitted, .resid)) +
		geom_point() +
		stat_smooth(method = "loess") +
		geom_hline(yintercept = 0, col = "tomato3", linetype = 'dashed') +
		labs(x = 'Fitted values', y = 'Residuals', title = sprintf('Residual vs Fitted Plot of Model%s', number)) +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

qqplot <- function(model, number = "") {

	# TODO: qqnorm produces extra plot
	resultQQ <- qqnorm(model$residuals)

	plot <- ggplot(model, aes(x = resultQQ$x, y = resultQQ$y)) +
		geom_point() +
		labs(x = 'Theoretical Quantiles', y = 'Standardized Residuals',
			 title = sprintf('Normal Q-Q of Model%s', number)) +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

scaleLocation <- function(model, number = "") {

	plot <- ggplot(model, aes(.fitted, sqrt(abs(.stdresid)))) +
		geom_point() +
		stat_smooth(method = "loess") +
		labs(x = 'Fitted Value', y = expression(sqrt("|Standardized residuals|")),
			 title = sprintf('Scale-Locationof of Model%s', number)) +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

cooks <- function(model, number = "") {

	plot <- ggplot(model, aes(seq_along(.cooksd), .cooksd)) +
		geom_bar(stat = "identity", position = "identity", fill = 'steelblue', color = 'black', alpha = 0.9) +
    	labs(x = "Obs. Number", y = "Cook's distance", title = sprintf("Cook's distance of Model%s", number)) +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

residualssVsLavarage <- function(model, number = "") {

	plot <- ggplot(model, aes(.hat, .stdresid)) +
		geom_point(aes(size = .cooksd)) +
		stat_smooth(method = "loess") +
		labs(x = "Leverage", y = "tandardized Residuals",
			 title = sprintf("Residual vs Leverage Plot of Model%s", number)) +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}
cooksVsLavarage <- function(model, number = "") {

	plot <- ggplot(model, aes(.hat, .cooksd)) +
		geom_point()+
		stat_smooth(method = "loess") +
		labs(x = "Leverage hii", y = "Cook's distance",
			 title = sprintf("Cook's dist vs Leverage hii/(1-hii) of Model%s", number)) +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

grid.arrange(residualsVsFitted(fit), residualsVsFitted(fit1, 1),
			 qqplot(fit), qqplot(fit1, 1),
			 cooks(fit), cooks(fit1, 1),
			 scaleLocation(fit), scaleLocation(fit1, 1),
			 cooksVsLavarage(fit), cooksVsLavarage(fit1, 1),
			 residualssVsLavarage(fit), residualssVsLavarage(fit1, 1),
			 ncol = 2, nrow = 6)

# Some metrics
list(model = summary(fit), model1 = summary(fit1))	# Adding weight as second predictor slightly improve R-squared
list(model = anova(fit), model1 = anova(fit1))
list(model = AIC(fit), model1 = AIC(fit1))	# Model with smaller AIC score is preferrable

