##' Robin Elahi
##' 9 Nov 2017
##' 23 April 2018
##' R-Ecology-Lesson
##' 02-starting-with-data

##### OBJECTIVES #####
# Describe what a data frame is.
# Load external data from a .csv file into a data frame.
# Summarize the contents of a data frame.
# Describe what a factor is.
# Convert between strings and factors.
# Reorder and rename factors.
# Change how character strings are handled in a data frame.
# Format dates.

##### PRESENTATION OF THE SURVEY DATA #####

# download.file("https://ndownloader.figshare.com/files/2292169",
#               "data/portal_data_joined.csv")
#               
surveys <- read.csv("data/portal_data_joined.csv")

# Check output
surveys
head(surveys)
View(surveys)

# read.csv
# read.csv2 (uses ; instead of ,)
# read.table (for tab delimited)

##### WHAT ARE DATAFRAMES? #####

# So what are dataframes?
# defacto structure for most tabular data
# can make by hand
# but usually read in a file
# columns are vectors that all have the same length

## Inspecting dataframes
head(surveys)
str(surveys)
dim(surveys)
nrow(surveys)
ncol(surveys)
names(surveys)
rownames(surveys)

str(surveys)

### Challenge 1

## Based on the output of str(surveys), can you answer the following questions?
# What is the class of the object surveys?
# How many rows and how many columns are in this object?
# How many species have been recorded during these surveys?

str(surveys)
## * class: data frame
## * how many rows: 34786,  how many columns: 13
## * how many species: 48

##### INDEXING AND SUBSETTING #####

surveys[1, 1]   # first element in the first column of the data frame (as a vector)
surveys[1, 6]   # first element in the 6th column (as a vector)
surveys[, 1]    # first column in the data frame (as a vector)
surveys[1]      # first column in the data frame (as a data.frame)
surveys[1:3, 7] # first three elements in the 7th column (as a vector)
surveys[3, ]    # the 3rd element for all columns (as a data.frame)
head_surveys <- surveys[1:6, ] # equivalent to head_surveys <- head(surveys)

surveys[,-1]          # The whole data frame, except the first column
surveys[-c(7:34786),] # Equivalent to head(surveys)

surveys["species_id"]       # Result is a data.frame
surveys[, "species_id"]     # Result is a vector
surveys[["species_id"]]     # Result is a vector
surveys$species_id          # Result is a vector

### Challenge 2

## Create a data.frame (surveys_200) containing only the observations from row 200 of the surveys dataset.

## Notice how nrow() gave you the number of rows in a data.frame?
# Use that number to pull out just that last row in the data frame.
# Compare that with what you see as the last row using tail() to make sure it's meeting expectations.
# Pull out that last row using nrow() instead of the row number.
# Create a new data frame (surveys_last) from that last row.

## Use nrow() to extract the row that is in the middle of the data frame. Store the content of this row in an object named surveys_middle.

## Combine nrow() with the - notation above to reproduce the behavior of head(surveys), keeping just the first through 6th rows of the surveys dataset.

### Answers
# 1. 
surveys_200 <- surveys[200, ]
# 2. 
nrow(surveys)
surveys[34786, ]
tail(surveys)
surveys[nrow(surveys), ]
surveys_last <- surveys[nrow(surveys), ]
# 3. 
surveys_middle <- surveys[nrow(surveys)/2, ]
# 4. 
surveys[-c(7:nrow(surveys)), ]
# 5. Use dim() and index operation to get the last row
surveys[dim(surveys)[1],]

##### FACTORS #####

## Note that columns genus, species, etc are factors
str(surveys)

## Factors are:
# special data class that is really useful for data visualization and analysis
# stored as integers associated with labels
# can be ordered, or unordered
# don't be fooled! factors are NOT strings, or character vectors
# once created, factors can only contain a pre-defined set of values, known as levels
# by default, R sorts levels alphabetically

sex <- factor(c("male", "female", "female", "male"))

# Integers are 1 and 2 - female and male (ABC)

levels(sex)
nlevels(sex)

# Sometimes the order does not matter, but sometimes it does
# e.g., low,  medium, high (2, 3, 1)

# Reorder sex
sex
sex <- factor(sex, levels = c("male", "female"))
sex # after re-ordering

# Converting factors (here to a character)
as.character(sex)

# Converting factors when the levels appear as numbers (e.g., years) is trickier
f <- factor(c(1990, 1983, 1977, 1998, 1990))
f
as.numeric(f)               # Wrong! And there is no warning...
as.character(f)
as.numeric(as.character(f)) # Works...
# better way
levels(f)
as.numeric(levels(f))
as.numeric(levels(f))[f]    # The recommended way (not sure why)

## In the levels() approach:
# we obtain all factor levels using levels()
# convert these to numeric values using as.numeric(levels(f))
# we then access these numeric values using the underlying integers of the vector f inside the square brackets

## Renaming factors
# Use plot() to get a quick glance of the number of observations of each factor level
plot(surveys$sex)
head(surveys)
tail(surveys)

# Note that ~1700 individuals have no sex information
# Additionally, there is no label (NA? undetermined?)
# Let's rename this label to something more meaningful
# Let's pull out the data on sex as an independent vector
sex <- surveys$sex
head(sex)
sex
levels(sex)

levels(sex)[1] <- "undetermined"
levels(sex)
head(sex)

### Challenge 3
## Rename "F" and "M" to "female" and "male" respectively.
## Now that we have renamed the factor level to "undetermined", can you recreate the
## barplot such that "undetermined" is last (after "male")?

levels(sex)[2:3] <- c("female", "male")
sex <- factor(sex, levels = c("female", "male", "undetermined"))
plot(sex)

## USING STRINGSASFACTORS = FALSE
## Compare the difference between when the data are being read as
## `factor`, and when they are being read as `character`.
surveys <- read.csv("data/portal_data_joined.csv", stringsAsFactors = TRUE)
str(surveys)
surveys <- read.csv("data/portal_data_joined.csv", stringsAsFactors = FALSE)
str(surveys)
## Convert the column "plot_type" into a factor
surveys$plot_type <- factor(surveys$plot_type)
str(surveys)

### Challenge 4

##  There are a few mistakes in this hand-crafted `data.frame`,
##  can you spot and fix them? Don't hesitate to experiment!

# animal_data <- data.frame(
#   animal = c(dog, cat, sea cucumber, sea urchin),
#   feel = c("furry", "squishy", "spiny"),
#   weight = c(45, 8 1.1, 0.8)
# )

animal_data <- data.frame(animal = c("dog", "cat", "sea cucumber", "sea urchin"),
                          feel = c("furry", "furry", "squishy", "spiny"),
                          weight = c(45, 8, 1.1, 0.8))
animal_data

##   Can you predict the class for each of the columns in the following example?
##   Check your guesses using `str(country_climate)`:

country_climate <- data.frame(country = c("Canada", "Panama", "South Africa", "Australia"),
                              climate = c("cold", "hot", "temperate", "hot/temperate"),
                              temperature = c(10, 30, 18, "15"),
                              northern_hemisphere = c(TRUE, TRUE, FALSE, "FALSE"),
                              has_kangaroo = c(FALSE, FALSE, FALSE, 1))

##   Are they what you expected? Why? why not?
##   What would have been different if we had added `stringsAsFactors = FALSE`
##     when we created this data frame?
##   What would you need to change to ensure that each column had the
##     accurate data type?

## Answers
# * missing quotations around the names of the animals
# * missing one entry in the "feel" column (probably for one of the furry animals)
# * missing one comma in the weight column
# * `country`, `climate`, `temperature`, and `northern_hemisphere` are
# factors; `has_kangaroo` is numeric
# * using `stringsAsFactors = FALSE` would have made them character instead of
# factors
# * removing the quotes in temperature and northern_hemisphere and replacing 1
# by TRUE in the `has_kangaroo` column would give what was probably 
# intended

##### FORMATTING DATES #####

## Best practices for storing dates
## 2018-04-01
## Or, separate columns for year, month, day

str(surveys)

## We are going to use the ymd() function in the package 'lubridate'
library(lubridate)

## takes a vector with year, month, day components and converts to a 'Date' vector
## 'Date' is a class of data recognized by R as being a date and can be manipulated as such
## 
?ymd()

### Personal faded example
# Create a string using paste
x <- paste(2017, 11, 15, sep = "-")
x

# This string can be turned into a date
x_date <- ymd(x)
str(x_date)

# Now do the same using vectors
yrs <- c(1999, 2012, 1980)
mths <- c(1, 3, 11)
dys <- c(28, 3, 5)

# Question
# ymd(paste(yrs, ___, ___, sep = ___))

# What happens when there are data missing?
ymd(paste(2017, NA, 15, sep = "-"))

## Let's create a date object and inspect the structure:
my_date <- ymd("2015-01-01")
str(my_date)

## Now let's paste the year, month, and day separately - we get the same result:
# sep indicates the character to use to separate each component
my_date <- ymd(paste("2015", "1", "1", sep = "-")) 
str(my_date)

# Now we apply this function to the surveys dataset. 
# Create a character vector from the `year`, `month`, and `day` columns of `surveys` using `paste()`:
paste(surveys$year, surveys$month, surveys$day, sep = "-")

# This character vector can be used as the argument for `ymd()`:
ymd(paste(surveys$year, surveys$month, surveys$day, sep = "-"))

# The resulting `Date` vector can be added to `surveys` as a new column called `date`:
surveys$date <- ymd(paste(surveys$year, surveys$month, surveys$day, sep = "-"))
str(surveys) # notice the new column, with 'date' as the class

# Let's make sure everything worked correctly. One way to inspect the new column is to use `summary()`:
summary(surveys$date)

# Something went wrong: some dates have missing values. Let's investigate where they are coming from.

# We can use the functions we saw previously to deal with missing data to identify
# the rows in our data frame that are failing. 
# If we combine them with what we learned about subsetting data frames earlier, we can extract the columns "year, "month", "day" from the records that have `NA` in our new column `date`. We will also use `head()` so we don't clutter the output:
is_missing_date <- is.na(surveys$date)
missing_dates <- surveys[is_missing_date, ]
head(missing_dates)

# Get fancy
date_columns <- c("year", "month", "day")
missing_dates <- surveys[is_missing_date,  date_columns]
head(missing_dates)

# Why did these dates fail to parse? If you had to use these data for your
# analyses, how would you deal with this situation?
# because april and september each only have 30 days, so this was a typo
