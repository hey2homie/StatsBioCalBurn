library(tidyverse)
library(gridExtra)

dataframe <- read.table('./muscle-incomplete.txt', header = TRUE)
dataframe <- na.omit(dataframe)		# Omiting NAs to carry out complete case analysis

# Exploring the results of linear regression in case of complete analysis
fit <- lm(calories ~ calhour, data = dataframe)
fit1 <- lm(calories ~ calhour + weight, data = dataframe)

list(model = summary(fit), model1 = summary(fit1))	# Adding weight as second predictor slightly improve R-squared
list(model = anova(fit), model1 = anova(fit1))
list(model = AIC(fit), model1 = AIC(fit1))	# Model with smaller AIC score is preferrable

dataframe$predicted1 <- predict(fit1)
dataframe$residuals1 <- residuals(fit1)

# Diagnosis plots
model.plot <- function(df, model, pred) {

	plot <- ggplot(df, aes(x = calhour, y = calories)) +
		geom_point(aes(alpha = abs(model$residuals), color = abs(model$residuals), size = abs(model$residuals))) +	# Plot actual values
		scale_color_continuous(low = 'black', high = 'red') +
		geom_point(aes(y = pred), col = 'steelblue') +	# Plot values predicted by first model
		geom_segment(aes(xend = calhour, yend = pred), alpha = 0.5) +
		labs(x = 'Calories per Hour', y = 'Heat Production',
			 title = 'Visualization of linear model with two predictors') +
		# TODO: Cannot add legend
		scale_fill_manual(name = ' ', values = c('steelblue', 'tomato3'), labels = c('Predicted', 'Actual')) +
		guides(alpha = FALSE, color = FALSE, size = FALSE) +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

model.plot(dataframe, fit1, dataframe$predicted1)

plotResiduals <- function(model) {

	plot <- ggplot(model, aes(x = model$residuals)) +
		geom_histogram(fill = 'steelblue', color = 'black', alpha = 0.9) +
		labs(x = 'Residual Values', y = 'Frequency', title = 'Residuals Destribution') +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

plotResiduals(fit1)

residualsVsFitted <- function(model) {

	plot <- ggplot(model, aes(.fitted, .resid)) +
		geom_point() +
		stat_smooth(method = "loess") +
		geom_hline(yintercept = 0, col = "tomato3", linetype = 'dashed') +
		labs(x = 'Fitted values', y = 'Residuals', title = 'Residual vs Fitted') +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

qqplot <- function(model) {

	# TODO: qqnorm produces extra plot
	resultQQ <- qqnorm(model$residuals)

	plot <- ggplot(model, aes(x = resultQQ$x, y = resultQQ$y)) +
		geom_point() +
		labs(x = 'Theoretical Quantiles', y = 'Standardized Residuals', title = 'Normal Q-Q') +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

scaleLocation <- function(model) {

	plot <- ggplot(model, aes(.fitted, sqrt(abs(.stdresid)))) +
		geom_point() +
		stat_smooth(method = "loess") +
		labs(x = 'Fitted Value', y = expression(sqrt("|Standardized residuals|")),
			 title = 'Scale-Locationof of Model%s') +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

cooks <- function(model) {

	plot <- ggplot(model, aes(seq_along(.cooksd), .cooksd)) +
		geom_bar(stat = "identity", position = "identity", fill = 'steelblue', color = 'black', alpha = 0.9) +
    	labs(x = "Obs. Number", y = "Cook's distance", title = "Cook's distance of Model%s") +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

residualssVsLavarage <- function(model, number = "") {

	plot <- ggplot(model, aes(.hat, .stdresid)) +
		geom_point(aes(size = .cooksd)) +
		stat_smooth(method = "loess") +
		labs(x = "Leverage", y = "tandardized Residuals",
			 title = "Residual vs Leverage Plot of Model%s") +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

cooksVsLavarage <- function(model, number = "") {

	plot <- ggplot(model, aes(.hat, .cooksd)) +
		geom_point()+
		stat_smooth(method = "loess") +
		labs(x = "Leverage hii", y = "Cook's distance",
			 title = "Cook's dist vs Leverage hii/(1-hii)") +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

grid.arrange(residualsVsFitted(fit1),
			 qqplot(fit1),
			 cooks(fit1),
			 scaleLocation(fit1),
			 cooksVsLavarage(fit1),
			 residualssVsLavarage(fit1),
			 ncol = 3, nrow = 2)

