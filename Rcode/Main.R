library(tidyverse)
library(GGally)
library(gridExtra)

dataframe <- read.table('./muscle-incomplete.txt', header = TRUE)

# Descriptive statistics
dataframe %>% select(weight, calhour, calories) %>%
	pivot_longer(data = ., cols = c(weight, calhour, calories), names_to = 'Variables', values_to = 'Values') %>%
	ggplot(aes(x = Variables, y = Values)) +
	geom_boxplot(color = 'black', fill = 'steelblue', alpha = 0.9) +
	labs(title = 'Boxplots of available variables') +
	theme_classic() +
	theme(plot.title = element_text(hjust = 0.5))
# No outliners

dataframe %>% ggpairs(data = ., ) + theme(panel.grid.major = element_blank())

# There is high positive correlation between variables calories and calories per hour
dataframe %>% ggplot(data = ., aes(x = calories, y = calhour)) +
	geom_point() +
	geom_smooth(method = 'lm', se = FALSE) +
	labs(x = 'Burned Calories', y = 'Calories per Hour',
		 title = 'Relationship between heat production and work levels') +
	theme_classic() +
	theme(plot.title = element_text(hjust = 0.5))

# Exploring missing data by visual aids
missing_values <- dataframe %>%
  	gather(key = 'key', value = 'val') %>%
  	mutate(isna = is.na(val)) %>%
  	group_by(key) %>%
	mutate(total = n()) %>%
	group_by(key, total, isna) %>%
	summarise(num.isna = n()) %>%
	mutate(pct = num.isna / total * 100)

# Plot of missing values by variable in percents
percentage.plot <- ggplot(data = missing_values) +
	geom_bar(aes(x = reorder(key, desc(pct)), y = pct, fill = isna), stat = 'identity', alpha = 0.9) +
	scale_fill_manual(name = '', values = c('steelblue', 'tomato3'), labels = c('Present', 'Missing')) +
	coord_flip() +
	labs(title = 'Percentage of missing values', x = 'Variables', y = '% of missing values') +
	theme_classic() +
	theme(plot.title = element_text(hjust = 0.5))

# Distiburion of missing values by rows
distibution.plot <- dataframe %>%
	mutate(id = row_number()) %>%
	gather(-id, key = 'key', value = 'val') %>%
	mutate(isna = is.na(val)) %>%
	ggplot(aes(key, id, fill = isna)) +
		geom_raster(alpha = 0.9) +
    	scale_fill_manual(name = '', values = c('steelblue', 'tomato3'), labels = c('Present', 'Missing')) +
    	labs(x = 'Variables', y = 'Row Number', title = 'Missing values in rows') +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5)) +
		coord_flip()

grid.arrange(percentage.plot, distibution.plot, ncol = 2)	# Display two plots in one window

dataframe <- na.omit(dataframe)		# Omiting NAs to carry out complete case analysis

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
		scale_fill_manual(name = ' ', values = c('steelblue', 'tomato3'), labels = c('Predicted', 'Actual')) +	# Doens't work for some reason
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
		labs(x = 'Residual Values', y = 'Frequency', title = sprintf('Residuals Destribution Model', number)) +
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
		labs(x = 'Fitted values', y = 'Residuals', title = sprintf('Residual vs Fitted Plot of model%s', number)) +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

qqplot <- function(model, number = "") {

	# TODO: qqnorm produces extra plots — need to remove them
	plot <- ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid)) +
		geom_point(na.rm = TRUE) +
    	geom_abline() +
		labs(x = 'Theoretical Quantiles', y = 'Standardized Residuals',
			 title = sprintf('Normal Q-Q of model%s', number)) +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot )
}

scaleLocation <- function(model, number = "") {

	plot <- ggplot(model, aes(.fitted, sqrt(abs(.stdresid)))) +
		geom_point(na.rm = TRUE) +
		stat_smooth(method = "loess", na.rm = TRUE) +
		labs(x = 'Fitted Value', y = expression(sqrt("|Standardized residuals|")),
			 title = sprintf('Scale-Locationof of model%s', number)) +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

cooks <- function(model, number = "") {

	plot <- ggplot(model, aes(seq_along(.cooksd), .cooksd)) +
		geom_bar(stat = "identity", position = "identity", fill = 'steelblue', color = 'black', alpha = 0.9) +
    	labs(x = "Obs. Number", y = "Cook's distance", title = sprintf("Cook's distance of model%s", number)) +
		theme_classic() +
		theme(plot.title = element_text(hjust = 0.5))

	return(plot)
}

grid.arrange(residualsVsFitted(fit), residualsVsFitted(fit1, 1),
			 qqplot(fit), qqplot(fit1, 1),
			 cooks(fit), cooks(fit1, 1),
			 scaleLocation(fit), scaleLocation(fit1, 1),
			 ncol = 2, nrow = 4)

# Some metrics
list(model = summary(fit), model1 = summary(fit1))	# Adding weight as second predictor slightly improve R-squared
list(model = anova(fit), model1 = anova(fit1))
list(model = AIC(fit), model1 = AIC(fit1))	# Model with smaller AIC score is preferrable
