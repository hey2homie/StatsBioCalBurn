library(tidyverse)

dataframe <- read.table("./muscle-incomplete.txt", header = TRUE)

# Exploring missing data by visual aids
missing_values <- dataframe %>%
  	gather(key = "key", value = "val") %>%
  	mutate(isna = is.na(val)) %>%
  	group_by(key) %>%
	mutate(total = n()) %>%
	group_by(key, total, isna) %>%
	summarise(num.isna = n()) %>%
	mutate(pct = num.isna / total * 100)

percentage.plot <- ggplot() +
	geom_bar(data = missing_values, aes(x = reorder(key, desc(pct)), y = pct, fill = isna), stat = 'identity',
			 alpha = 0.9) +
	scale_fill_manual(name = "", values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
	coord_flip() +
	labs(title = "Percentage of missing values", x = 'Variable', y = "% of missing values") +
	theme_classic() +
	theme(plot.title = element_text(hjust = 0.5))

percentage.plot


