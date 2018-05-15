##' Robin Elahi
##' 12 May 2018
##' R-Ecology-Lesson
##' 04-visualization-ggplot2

##### OBJECTIVES #####

# Produce scatter plots, boxplots, and time series plots using ggplot.
# Set universal plot settings.
# Describe what faceting is and apply faceting in ggplot.
# Modify the aesthetics of an existing ggplot plot (including axis labels and color).
# Build complex and customized plots from data in a data frame.

##### INTRO - ggplot2 #####

##' Key points
##' complex, beautiful plots based on dataframes and a general 'grammar'
##' ggplot2 functions like tidy data in long format
##' (column for every dimension, a row for every observation)
##' well-structured data will save you lots of time

##' basic grammar template:
##' ggplot(data = , mapping = aes()) + GEOM_FUNCTION
##' ggplot(data = surveys_complete)
##' ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length))

##' Emphasis on visualizing the data
##' Scatterplot
##' Histogram
##' Boxplot
##' Means + standard deviations

library(tidyverse)
library(viridis) # for color-blind friendly plots

## Load surveys_complete data that we prepared last week
surveys_complete <- read_csv("carpentry_training/data_output/surveys_complete.csv")

##### GEOM_POINT #####

ggplot(data = surveys_complete, 
       mapping = aes(x = weight, y = hindfoot_length)) + geom_point()

## Save the mapping
surveys_plot <- ggplot(data = surveys_complete, aes(weight, hindfoot_length))

surveys_plot + geom_point()

## Add color to all the points, alpha
ggplot(data = surveys_complete, aes(weight, hindfoot_length)) + 
  geom_point(alpha = 0.1, color = "blue")

## Color by species_id
ggplot(data = surveys_complete, 
       aes(weight, hindfoot_length, color = species_id)) + 
  geom_point(alpha = 0.3) + 
  scale_color_viridis_d()

### Challenge 1 ###
# Use what you just learned to create a scatter plot of weight over species_id with the plot types showing in different colors. 
# Is this a good way to show this type of data?

ggplot(data = surveys_complete, aes(x = species_id, y = weight)) +
  geom_point(aes(color = plot_type))

##### GEOM_BOXPLOT #####

ggplot(data = surveys_complete, aes(species_id, weight)) + 
  geom_boxplot()

ggplot(data = surveys_complete, aes(species_id, weight)) + 
  geom_boxplot(alpha = 0) + 
  geom_jitter(alpha = 0.3, color = "tomato")

# Fix the layering
ggplot(data = surveys_complete, aes(species_id, weight)) + 
  geom_jitter(alpha = 0.3, color = "tomato") + 
  geom_boxplot(alpha = 0)

### Challenge 2 ###
##  Start with the boxplot we created:
ggplot(data = surveys_complete, aes(x = species_id, y = weight)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato")

##  1. Replace the box plot with a violin plot; see `geom_violin()`.

##  2. Represent weight on the log10 scale; see `scale_y_log10()`.

##  3. Create boxplot for `hindfoot_length` overlaid on a jitter layer.

##  4. Add color to the data points on your boxplot according to the
##  plot from which the sample was taken (`plot_id`).
##  *Hint:* Check the class for `plot_id`. Consider changing the class
##  of `plot_id` from integer to factor. Why does this change how R
##  makes the graph?  

# 1. 
ggplot(data = surveys_complete, aes(species_id, weight)) + 
  geom_jitter(alpha = 0.3, color = "tomato") + 
  geom_violin(alpha = 0)

# 2. 
ggplot(data = surveys_complete, aes(species_id, weight)) + 
  geom_jitter(alpha = 0.3, color = "tomato") + 
  geom_violin(alpha = 0) + 
  scale_y_log10()

# 3. 
ggplot(data = surveys_complete, 
       aes(species_id, hindfoot_length)) + 
  geom_jitter(alpha = 0.3, color = "red") + 
  geom_boxplot(alpha = 0.7)

# 4. 
ggplot(data = surveys_complete, 
       aes(species_id, hindfoot_length, color = plot_id)) + 
  geom_jitter(alpha = 0.3) + 
  geom_boxplot(alpha = 0.7) 

ggplot(data = surveys_complete, 
       aes(species_id, hindfoot_length, 
           color = as.character(plot_id))) + 
  geom_jitter(alpha = 0.3) + 
  geom_boxplot(alpha = 0.7) 

## Reorder by weight
str(surveys_complete)
surveys_complete <- surveys_complete %>% 
  mutate(species_id = reorder(species_id, -weight, median))
str(surveys_complete)

ggplot(data = surveys_complete, 
       aes(species_id, weight)) + 
  geom_boxplot(alpha = 0) 

##### GEOM_HIST #####

surveys_complete %>% 
  ggplot(aes(weight)) + 
  geom_histogram() + 
  facet_wrap(~ species_id)

## Focus on one species
surveys_complete %>% 
  filter(species_id == "DM") %>%
  ggplot(aes(weight)) + 
  geom_histogram() + 
  facet_wrap(~ species_id)

## Focus on one species, by sex; adjust bin width
surveys_complete %>% 
  filter(species_id == "DM") %>%
  ggplot(aes(weight, fill = sex)) + 
  geom_histogram(alpha = 0.5, binwidth = 1) 

##### GEOM_LINE #####

### Time series data
yearly_counts <- surveys_complete %>% count(year, species_id)

yearly_counts

# Does not work
ggplot(yearly_counts, aes(year, n)) + 
  geom_line()

# Separate by species
ggplot(yearly_counts, aes(year, n, group = species_id)) + 
  geom_line()

# Add colors to species
ggplot(yearly_counts, aes(year, n, color = species_id)) + 
  geom_line()

# Or facet!
ggplot(yearly_counts, aes(year, n)) + 
  geom_line() + 
  facet_wrap(~ species_id)

# Split by sex
yearly_sex_counts <- surveys_complete %>% count(year, species_id, sex)

yearly_sex_counts %>% 
  ggplot(aes(year, n, color = sex)) + 
  geom_line() + 
  facet_wrap(~ species_id)

# Modify theme
yearly_sex_counts %>% 
  ggplot(aes(year, n, color = sex)) + 
  geom_line() + 
  facet_wrap(~ species_id) + 
  theme_bw() + 
  theme(panel.grid = element_blank())

# Select one species and add regression
yearly_counts %>% 
  filter(species_id == "PB") %>% 
  ggplot(aes(year, n)) + 
  geom_point() + 
  facet_wrap(~ species_id) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  geom_smooth(method = "lm")

#### SUMMARY STATS AND ERROR BARS ####

ggplot(data = surveys_complete, aes(species_id, weight)) + 
  geom_jitter(alpha = 0.3, color = "tomato") + 
  geom_violin(alpha = 0)

### Challenge 3 ###

## We often want to plot point estimates (e.g., mean, median) 
## along with a measure of uncertainty 
## (e.g., standard deviation, standard error, confidence interval) 

#' 1. First create a new dataframe, called 'surveys_summary', that summarises weight by species_id - calculate the mean, standard deviation, and sample size (n).

#' 2. Plot mean body weight against species_id

#' 3. Indicate the number of samples in the plot
#' hint 1: use the size argument in the aes() mapping)
#' hint 2: http://ggplot2.tidyverse.org/reference/geom_point.html

#' 4. On the previous plot, add error bars for the standard deviation
#' hint(use a new layer, called geom_errorbar())

#' 5. Recreate 'surveys_summary', but now add the standard error
#' (google the formula for standard error if you have not learned this or forgotten)
#' Now on the plot, add error bars for the standard deviation
#' hint(use a new layer, called geom_errorbar())

# 1. 
surveys_summary <- surveys_complete %>% 
  filter(!is.na(weight)) %>% 
  group_by(species_id) %>% 
  summarise(wt_mean = mean(weight), 
            wt_sd = sd(weight), 
            n = n())
surveys_summary

# 2.
surveys_summary %>% 
  ggplot(aes(species_id, wt_mean)) + 
  geom_point()

# 3. 
surveys_summary %>% 
  ggplot(aes(species_id, wt_mean)) + 
  geom_point(aes(size = n))

# 4. 
surveys_summary %>% 
  ggplot(aes(species_id, wt_mean)) + 
  geom_point(aes(size = n)) + 
  geom_errorbar(aes(ymin = wt_mean - wt_sd, 
                ymax = wt_mean + wt_sd))

# 5. Get standard errors 
surveys_summary <- surveys_complete %>% 
  filter(!is.na(weight)) %>% 
  group_by(species_id) %>% 
  summarise(wt_mean = mean(weight), 
            wt_sd = sd(weight), 
            n = n(), 
            wt_se = wt_sd / sqrt(n))

surveys_summary %>% 
  ggplot(aes(species_id, wt_mean)) + 
  geom_point(aes(size = n)) + 
  geom_errorbar(aes(ymin = wt_mean - wt_se, 
                    ymax = wt_mean + wt_se))


## Introduce 95% CIs and plot those
surveys_summary <- surveys_complete %>% 
  filter(!is.na(weight)) %>% 
  group_by(species_id) %>% 
  summarise(wt_mean = mean(weight), 
            wt_sd = sd(weight), 
            n = n(), 
            wt_se = wt_sd / sqrt(n), 
            wt_ci = 1.96 * wt_se)

surveys_summary %>% 
  ggplot(aes(species_id, wt_mean)) + 
  geom_point(aes(size = n)) + 
  geom_errorbar(aes(ymin = wt_mean - wt_ci, 
                    ymax = wt_mean + wt_ci))

## Add in standard deviations and confidence intervals
surveys_summary %>% 
  ggplot(aes(species_id, wt_mean)) + 
  geom_point(aes()) + 
  geom_errorbar(aes(ymin = wt_mean - wt_sd, 
                    ymax = wt_mean + wt_sd), 
                width = 0, size = 0.5) + 
  geom_errorbar(aes(ymin = wt_mean - wt_ci, 
                    ymax = wt_mean + wt_ci), 
                width = 0, size = 1, color = "red")

#### EXPORTING ####

surveys_summary %>% 
  ggplot(aes(species_id, wt_mean)) + 
  geom_point(aes(size = n)) + 
  geom_errorbar(aes(ymin = wt_mean - wt_sd, 
                    ymax = wt_mean + wt_sd)) + 
  theme_bw(base_size = 14)

ggsave("carpentry_training/fig_output/fig1.pdf", 
       height = 3.5, width = 5)

### Challenge 4 ###
## Tweaking the plot
## Try to change axis labels; add figure title (hint: labs())
## Try to remove the gray panel gridlines (hint: theme())
## Try to change the error bar widths (hint: geom_errorbar())
## Try to place the legend on the bottom of the figure
# https://www.rstudio.com/wp-content/uploads/2015/08/ggplot2-cheatsheet.pdf

surveys_summary %>% 
  ggplot(aes(species_id, wt_mean)) + 
  geom_point(aes(size = n)) + 
  geom_errorbar(aes(ymin = wt_mean - wt_sd, 
                    ymax = wt_mean + wt_sd), 
                width = 0, size = 0.5) + 
  labs(title = "Portal LTER mammal species", 
       x = "Species code", 
       y = "Weight (g)") + 
  theme_bw(base_size = 14) + 
  theme(panel.grid = element_blank()) + 
  theme(legend.position = "bottom")

ggsave("carpentry_training/fig_output/fig1.pdf", 
       height = 3.5, width = 5)

### Challenge 5 ###
## Try to add the original data points to the plot
## Hint: use geom_jitter as before

surveys_summary %>% 
  ggplot(aes(species_id, wt_mean)) + 
  geom_jitter(data = surveys_complete, aes(species_id, weight), 
              alpha = 0.25, size = 0.75, width = 0.1, color = "lightblue") + 
  geom_point(aes()) + 
  geom_errorbar(aes(ymin = wt_mean - wt_sd, 
                    ymax = wt_mean + wt_sd), 
                width = 0, size = 0.5) + 
  labs(title = "Portal LTER mammal species", 
       x = "Species code", 
       y = "Weight (g)") + 
  theme_bw(base_size = 14) + 
  theme(panel.grid = element_blank()) + 
  theme(legend.position = "bottom")

ggsave("carpentry_training/fig_output/fig1.pdf", 
       height = 3.5, width = 5)

#### EXTRAS...#####

## Themes
#install.packages("ggthemes")
library(ggthemes)

my_plot <- yearly_sex_counts %>% 
  ggplot(aes(year, n, color = sex)) + 
  geom_line() + 
  facet_wrap(~ species_id)

# my_plot + theme_few() 
# my_plot + theme_par() 
my_plot + theme_tufte() 
my_plot + theme_wsj() 
my_plot + theme_fivethirtyeight() 
my_plot + theme_excel()


# Challenge 6
# 
# With all of this information in hand, please take another five minutes to either improve one of the plots generated in this exercise or create a beautiful graph of your own. Use the RStudio ggplot2 cheat sheet for inspiration. 

# https://www.rstudio.com/wp-content/uploads/2015/08/ggplot2-cheatsheet.pdf

# Here are some ideas:
#   
#   See if you can change the thickness of the lines.
# Can you find a way to change the name of the legend? What about its labels?
#   Try using a different color palette (see http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/).