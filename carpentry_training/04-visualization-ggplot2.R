##### 04-visualization-ggplot2 #####

library(tidyverse)
library(hexbin)

read_csv("carpentry_training/data_output/surveys_complete.csv")

surveys_plot <- ggplot(data = surveys_complete, aes(weight, hindfoot_length))

## 
surveys_plot + geom_point()
surveys_plot + geom_hex()


ggplot(data = surveys_complete, aes(weight, hindfoot_length)) + 
  geom_point(alpha = 0.1, color = "blue")

ggplot(data = surveys_complete, aes(weight, hindfoot_length, color = species_id)) + 
  geom_point(alpha = 0.1)

ggplot(data = surveys_complete, aes(species_id, weight)) + 
  geom_boxplot()

ggplot(data = surveys_complete, aes(species_id, weight)) + 
  geom_jitter(alpha = 0.3, color = "tomato") + 
  geom_boxplot(alpha = 0)

### Time series data

yearly_counts <- surveys_complete %>%
  group_by(year, species_id) %>% 
  tally()

yearly_counts

ggplot(yearly_counts, aes(year, n)) + 
  geom_line()

ggplot(yearly_counts, aes(year, n, group = species_id)) + 
  geom_line()

ggplot(yearly_counts, aes(year, n, color = species_id)) + 
  geom_line()

ggplot(yearly_counts, aes(year, n)) + 
  geom_line() + 
  facet_wrap(~ species_id)

yearly_sex_counts <- surveys_complete %>%
  group_by(year, species_id, sex) %>% 
  tally()

yearly_sex_counts %>% 
  ggplot(aes(year, n, color = sex)) + 
  geom_line() + 
  facet_wrap(~ species_id)

yearly_sex_counts %>% 
  ggplot(aes(year, n, color = sex)) + 
  geom_line() + 
  facet_wrap(~ species_id) + 
  theme_minimal() + 
  theme(panel.grid = element_blank())

## Themes
install.packages("ggthemes")
library(ggthemes)

my_plot <- yearly_sex_counts %>% 
  ggplot(aes(year, n, color = sex)) + 
  geom_line() + 
  facet_wrap(~ species_id)

my_plot + theme_few() 
my_plot + theme_par() 
my_plot + theme_tufte() 
my_plot + theme_wsj() 
my_plot + theme_fivethirtyeight() 
my_plot + theme_base() 

##
yearly_sex_weight <- surveys_complete %>%
  group_by(year, sex, species_id) %>%
  summarize(avg_weight = mean(weight))

ggplot(data = yearly_sex_weight, aes(x = year, y = avg_weight, color = species_id)) +
  geom_line() +
  facet_grid(sex ~ .)

ggplot(data = yearly_sex_weight, aes(x = year, y = avg_weight, color = species_id)) +
  geom_line() +
  facet_grid(. ~ sex)

## gridextra

library(gridExtra)

spp_weight_boxplot <- ggplot(data = surveys_complete, aes(x = species_id, y = weight)) +
  geom_boxplot() +
  xlab("Species") + ylab("Weight (g)") +
  scale_y_log10()

spp_count_plot <- ggplot(data = yearly_counts, aes(x = year, y = n, color = species_id)) +
  geom_line() + 
  xlab("Year") + ylab("Abundance")

grid.arrange(spp_weight_boxplot, spp_count_plot, ncol = 2, widths = c(4, 6))
