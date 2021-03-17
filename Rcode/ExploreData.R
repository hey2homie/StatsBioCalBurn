library(tidyverse)
library(GGally)

dataframe <- read.table('./muscle-incomplete.txt', header = TRUE)

# Exploring missing data by visual aids
missing_values <- dataframe %>%
  	gather(key = 'key', value = 'val') %>%
  	mutate(isna = is.na(val)) %>%
  	group_by(key) %>%
	mutate(total = n()) %>%
	group_by(key, total, isna) %>%
	summarise(num.isna = n()) %>%
	mutate(pct = num.isna / total * 100)

percentage.plot <- ggplot(data = missing_values) +
	geom_bar(aes(x = reorder(key, desc(pct)), y = pct, fill = isna), stat = 'identity', alpha = 0.9) +
	scale_fill_manual(name = '', values = c('steelblue', 'tomato3'), labels = c('Present', 'Missing')) +
	coord_flip() +
	labs(title = 'Percentage of missing values', x = 'Variables', y = '% of missing values') +
	theme_classic() +
	theme(plot.title = element_text(hjust = 0.5))

percentage.plot

dataframe <- na.omit(dataframe)

# Descriptive statistics
dataframe %>% select(weight, calhour, calories) %>%
	pivot_longer(data = ., cols = c(weight, calhour, calories), names_to = 'Variables', values_to = 'Values') %>%
	ggplot(aes(x = Variables, y = Values)) +
	geom_boxplot(color = 'black', fill = 'steelblue') +
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

# Exploring the results of linear regression
model <- lm(calhour ~ calories, data = dataframe)
summary(model)
model1 <- lm(calhour ~ calories + weight, data = dataframe)
summary(model1)	# Adding weight as second predictor slightly improve R-squared

# ExploreData.R
# This R file will analyse the data with visual tools.

## setting the working directory
install.packages('rstudioapi')
library("rstudioapi") 
## set working directory
setwd(dirname(getActiveDocumentContext()$path))
## read data
dataframe <- read.table('../muscle-incomplete.txt', header = TRUE)

## packages needed for analysis
install.packages('tidyverse')
install.packages('GGally')
library(tidyverse)
library(GGally)

# Exploring missing data by visual aids
missing_values <- dataframe %>%
  gather(key = 'key', value = 'val') %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
	group_by(key, total, isna) %>%
	summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

percentage.plot <- ggplot(data = missing_values) +
	geom_bar(aes(x = reorder(key, desc(pct)), y = pct, fill = isna), stat = 'identity', alpha = 0.9) +
	scale_fill_manual(name = '', values = c('steelblue', 'tomato3'), labels = c('Present', 'Missing')) +
	coord_flip() +
	labs(title = 'Percentage of missing values', x = 'Variables', y = '% of missing values') +
	theme_classic() +
	theme(plot.title = element_text(hjust = 0.5))

percentage.plot

dataframe <- na.omit(dataframe)

# Descriptive statistics
dataframe %>% select(weight, calhour, calories) %>%
	pivot_longer(data = ., cols = c(weight, calhour, calories), names_to = 'Variables', values_to = 'Values') %>%
	ggplot(aes(x = Variables, y = Values)) +
	geom_boxplot(color = 'black', fill = 'steelblue') +
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

# Exploring the results of linear regression
model <- lm(calhour ~ calories, data = dataframe)
summary(model)
model1 <- lm(calhour ~ calories + weight, data = dataframe)
summary(model1)	# Adding weight as second predictor slightly improve R-squared


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


