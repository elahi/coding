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

summary(surveys_hindfoot_half)

##### SPLIT-APPLY-COMBINE #####

## split the data into groups (i.e., need categorical variables)
## apply some analysis to each group
## combine the results
## group_by() is often paired with summarise()

## By sex
surveys %>% 
  group_by(sex) %>% 
  summarise(mean_weight = mean(weight, na.rm = TRUE))

## By multiple columns
surveys %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight, na.rm = TRUE)) %>% summary()

## We can remove NAs first:
surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight))

## Display more data
surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight)) %>% 
  print(n = 15)

## Summarise multiple variables at a time!!
surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight), 
            min_weight = min(weight))

## Arrange results (smallest first)
surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight), 
            min_weight = min(weight)) %>% 
  arrange(min_weight)

## Arrange results (smallest last)
surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight), 
            min_weight = min(weight)) %>% 
  arrange(desc(min_weight))

##### COUNT #####

## we often want to know the number of observations per combination of factors
## use count
surveys %>% count(sex)
surveys %>% count(sex, sort = TRUE)

### Challenge 3
##  1. How many individuals were caught in each `plot_type` surveyed?

##  2. Use `group_by()` and `summarize()` to find the mean, min, and max
## hindfoot length for each species (using `species_id`). Also add the number of
## observations (hint: see `?n`).

##  3. What was the heaviest animal measured in each year? Return the
##  columns `year`, `genus`, `species_id`, and `weight`.

## 1. 
surveys %>% count(plot_type)
## 2. 
surveys %>%
  filter(!is.na(hindfoot_length)) %>%
  group_by(species_id) %>%
  summarize(
    mean_hindfoot_length = mean(hindfoot_length),
    min_hindfoot_length = min(hindfoot_length),
    max_hindfoot_length = max(hindfoot_length),
    n = n()
  )
## 3. 
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(year) %>%
  filter(weight == max(weight)) %>%
  select(year, genus, species, weight) %>%
  arrange(year)

##### RESHAPING WITH GATHER AND SPREAD #####

## Tidy dataset - 4 rules
# 1. Each variable has its own column
# 2. Each observation has its own row
# 3. Each value must have its own cell
# 4. Each type of observational unit forms a table

# Is 'surveys' tidy?
View(surveys)

# Yes.

# What if instead we wanted to compare the different mean weight of each species?
# Let's look at the species
unique(surveys$species_id)
surveys %>% distinct(species_id, genus, species) %>% View()

# How would we do this?
# Create new table where each row (the unit) represents a plot
# with all of the associated columns representing the mean weight of each species

# First create the table of mean weights as we have already done:
# (but this time according to genus)
# Surveys - mean weight by genus
surveys_gw <- surveys %>%
  filter(!is.na(weight)) %>%
  group_by(genus, plot_id) %>%
  summarize(mean_weight = mean(weight))
surveys_gw

str(surveys_gw)

## Spreading
## spread(data, key, value)
## key = the column variable whose levels will become new column names
## value = the column variable whose values will fill the new column variables

surveys_spread <- surveys_gw %>% 
  spread(key = genus, value = mean_weight)

str(surveys_spread)

## fill (for NAs)
surveys_gw %>% 
  spread(key = genus, value = mean_weight, fill = 0) %>% 
  View()

## Show spread_image

## Gathering
## Can we take this 'wide' data that we just spread - and 'gather' it back into long format?
## gather(data, key, value, names)
## key = the column variable we wish to create from column names
## value = the column variable we wish to create and fill with values associated with the key
## names = the names of the columns we use to fill the key variable

## To recreate surveys_gw from surveys spread, we would have to create:
## key = 'genus'
## value = 'mean_weight'
## which columns?  all except for 'plot_id'

names(surveys_spread)

## simple
surveys_spread %>% 
  gather(key = genus, value = mean_weight, Baiomys)

## Select columns with :
surveys_gather <- surveys_spread %>%
  gather(key = genus, value = mean_weight, Baiomys:Spermophilus) 

## use drop
surveys_gather <- surveys_spread %>%
  gather(key = genus, value = mean_weight, -plot_id)
surveys_gather
str(surveys_gather)

### Challenge 4
## 1. Make a wide data frame with `year` as columns, `plot_id`` as rows, and where the values are the number of genera per plot. You will need to summarize before reshaping, and use the function `n_distinct` to get the number of unique genera within a chunk of data. It's a powerful function! See `?n_distinct` for more.

## 2. Now take that data frame, and make it long again, so each row is a unique `plot_id` `year` combination

## 3. The `surveys` data set is not truly wide or long because there are two columns of measurement - `hindfoot_length` and `weight`.  This makes it difficult to do things like look at the relationship between mean values of each measurement per year in different plot types. Let's walk through a common solution for this type of problem. First, use `gather` to create a truly long dataset where we have a key column called `measurement` and a `value` column that takes on the value of either `hindfoot_length` or `weight`. Hint: You'll need to specify which columns are being gathered.

## 4. With this new truly long data set, calculate the average of each `measurement` in each `year` for each different `plot_type`. Then `spread` them into a wide data set with a column for `hindfoot_length` and `weight`. Hint: Remember, you only need to specify the key and value columns for `spread`.

## 1. 
n_distinct(surveys$genus)
rich_time <- surveys %>%
  group_by(plot_id, year) %>% 
  summarise(n_genera = n_distinct(genus)) %>% 
  spread(year, n_genera)
rich_time

## 2. 
rich_time %>% 
  gather(key = year, value = n_genera, -plot_id)

rich_time %>% 
  gather(key = year, value = n_genera, `1977`:`2002`)

## 3. 
surveys_long <- surveys %>% 
  gather(key = measurement, value = value, hindfoot_length)

surveys_long <- surveys %>% 
  gather(key = measurement, value = value, hindfoot_length, weight)

surveys_long

## 4. 
surveys_long %>%
  group_by(year, measurement, plot_type) %>%
  summarize(mean_value = mean(value, na.rm = TRUE)) %>%
  spread(measurement, mean_value)

##### EXPORTING DATA #####

## create new folder for generated data
## raw data are precious!
## we are going to create a cleaned up version without NAs
dir.exists("data_output")
dir.create("data_output")


## remove species_id that are NA
## represented by an emptry string (not NA)
unique(surveys$species_id)
surveys %>% distinct(species_id)

surveys_complete <- surveys %>% 
  filter(!is.na(weight), 
         !is.na(hindfoot_length), 
         !is.na(sex))

## We are interested in plotting species abundances over time
## So: remove observations for rare species
## Two steps

## Extract the most common species_id
species_counts <- surveys_complete %>% 
  count(species_id) %>% 
  filter(n >= 50)

## Only keep the most common species
surveys_complete <- surveys_complete %>% 
  filter(species_id %in% species_counts$species_id)

## Check dimensions
dim(surveys_complete)

write_csv(surveys_complete, path = "data_output/surveys_complete.csv")






