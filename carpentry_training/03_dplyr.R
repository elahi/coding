##' Robin Elahi
##' 2 May 2018
##' R-Ecology-Lesson
##' 03-dplyr

##### OBJECTIVES #####
# Describe the purpose of the **`dplyr`** and **`tidyr`** packages.
# Select certain columns in a data frame with the **`dplyr`** function `select`.
# Select certain rows in a data frame according to filtering conditions with the **`dplyr`** function `filter` .
# Link the output of one **`dplyr`** function to the input of another function with the 'pipe' operator `%>%`.
# Add new columns to a data frame that are functions of existing columns with `mutate`.
# Use the split-apply-combine concept for data analysis.
# Use `summarize`, `group_by`, and `count` to split a data frame into groups of observations, apply a summary statistics for each group, and then combine the results.
# Describe the concept of a wide and a long table format and for which purpose those formats are useful.
# Describe what key-value pairs are.
# Reshape a data frame from long to wide format and back with the `spread` and `gather` commands from the **`tidyr`** package.
# Export a data frame to a .csv file.

##### DATA MANIPULATION - INTRO #####

# dplyr - package for making tabular data manipulation *easy*
# package - sets of additional functions that let you do more stuff (need to install in every session)
# tidyverse - umbrella package

install.packages("tidyverse")
library(tidyverse)

# download.file("https://ndownloader.figshare.com/files/2292169",
#               "data/portal_data_joined.csv")
#    

surveys <- read_csv("data/portal_data_joined.csv")

## Inspect the data
str(surveys)
surveys

## Preview data
View(surveys)

## Notice the class 'tibble'
## dataframe, that tweaks some of the old bad behaviors of data frames
## 1. displays only the first 10 rows, and some columns
## 2. does not convert characters to factors

## We will learn:
## select
## filter
## mutate
## group_by
## summarise
## arrange
## count

## select
select(surveys, plot_id, species_id, weight)

## filter
filter(surveys, year == 1995)

## pipes
## what if you want to select and filter at the same time?
## Intermediate steps:
surveys2 <- filter(surveys, weight < 5)
surveys_sml <- select(surveys2, species_id, sex, weight) # readable, but clutters workspace

## Nest functions
surveys_sml <- select(filter(surveys, weight < 5), species_id, sex, weight) # handy, but not readable

## Pipes!  A recent addition to R! %>%
## control + shift + M (pc, mac)
## command + shift + M (mac)

surveys_sml <- surveys %>% 
  filter(weight < 5) %>% 
  select(species_id, sex, weight)

surveys_sml

### Challenge 1
##  Using pipes, subset the data to include individuals collected
##  before 1995, and retain the columns `year`, `sex`, and `weight.`

surveys %>%
  filter(year < 1995) %>%
  select(year, sex, weight)

##### MUTATE #####

## want to create new columns based on old ones
## use mutate()

## one new column
surveys2 <- surveys %>% 
  mutate(weight_kg = weight / 1000)

## two new columns
surveys2 <- surveys %>% 
  mutate(weight_kg = weight / 1000, 
         weight_kg2 = weight_kg * 2)

## remove NAs
surveys %>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight / 1000)

### Challenge 2:
##  Create a new data frame from the `surveys` data that meets the following
##  criteria: contains only the `species_id` column and a column that
##  contains values that are half the `hindfoot_length` values (e.g. a
##  new column `hindfoot_half`). In this `hindfoot_half` column, there are
##  no NA values and all values are < 30.

##  Hint: think about how the commands should be ordered to produce this data frame!
surveys_hindfoot_half <- surveys %>%
  filter(!is.na(hindfoot_length)) %>%
  mutate(hindfoot_half = hindfoot_length / 2) %>%
  filter(hindfoot_half < 30) %>%
  select(species_id, hindfoot_half)

##### SPLIT-APPLY-COMBINE #####

## split the data into groups
## apply some analysis to each group
## combine the results
## group_by() is often paired with summarise()




##### MUTATE #####
##### MUTATE #####
##### MUTATE #####

surveys_gw <- surveys %>%
  filter(!is.na(weight)) %>%
  group_by(genus, plot_id) %>%
  summarize(mean_weight = mean(weight))

surveys_gw
str(surveys_gw)

surveys_spread <- surveys_gw %>%
  spread(key = genus, value = mean_weight)

surveys_spread
str(surveys_spread)

surveys_gather <- surveys_spread %>%
  gather(key = genus, value = mean_weight, -plot_id)

str(surveys_gather)

surveys_spread %>%
  gather(key = genus, value = mean_weight)

surveys_complete <- surveys %>%
  filter(species_id != "",         # remove missing species_id
         !is.na(weight),           # remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         sex != "")                # remove missing sex


species_counts <- surveys_complete %>% 
  group_by(species_id) %>% 
  tally() %>% 
  filter(n >= 50)

surveys_complete <- surveys_complete %>% 
  filter(species_id %in% species_counts$species_id)

write_csv(surveys_complete, path = "carpentry_training/data_output/surveys_complete.csv")




