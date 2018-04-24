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
sex <- factor(c("male", "female", "female", "male"))

levels(sex)
nlevels(sex)

sex
sex <- factor(sex, levels = c("male", "female"))
sex # after re-ordering

as.character(sex)

f <- factor(c(1990, 1983, 1977, 1998, 1990))
as.numeric(f)               # Wrong! And there is no warning...
as.numeric(as.character(f)) # Works...
as.numeric(levels(f))[f]    # The recommended way.

as.numeric(levels(f))[f]

surveys$sex_char <- as.character(surveys$sex)
tail(surveys$sex_char)

## bar plot of the number of females and males captured during the experiment:
plot(surveys$sex)
plot(surveys$sex_char) # character


## Challenge
sex <- surveys$sex
head(sex)
sex
levels(sex)
levels(sex)[1] <- "missing"
levels(sex)
levels(sex)[2] <- "female"
levels(sex)[3] <- "male"
sex <- factor(sex, levels = c("female", "male", "missing"))
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

## Challenge

animal_data <- data.frame(animal = c("dog", "cat", "sea cucumber", "sea urchin"),
                          feel = c("furry", "furry", "squishy", "spiny"),
                          weight = c(45, 8, 1.1, 0.8))
animal_data


country_climate <- data.frame(
  country = c("Canada", "Panama", "South Africa", "Australia"),
  climate = c("cold", "hot", "temperate", "hot/temperate"),
  temperature = c(10, 30, 18, "15"),
  northern_hemisphere = c(TRUE, TRUE, FALSE, "FALSE"),
  has_kangaroo = c(FALSE, FALSE, FALSE, 1)
)
str(country_climate)

##### FORMATTING DATES #####

str(surveys)
library(lubridate)

paste(surveys$year, surveys$month, surveys$day, sep = "-")
# sep indicates the character to use to separate each component

ymd(paste(surveys$year, surveys$month, surveys$day, sep = "-"))

# No warning message
surveys$date <- ymd(paste(surveys$year, surveys$month, surveys$day, sep = "-"))
str(surveys)
summary(surveys)

# Fixed this in code
surveys_dates <- paste(surveys$year, surveys$month, surveys$day, sep = "-")
head(surveys_dates)

# Why did these dates fail to parse?
surveys_dates[is.na(surveys$date)] # because april and september each only have 30 days, so this was a typo


##### CREATE A FADED EXAMPLE FROM ABOVE CODE #####

# # Create a string using paste
# x <- paste(2017, 11, 15, sep = "-")
# 
# # This string can be turned into a date
# x_date <- ymd(x)
# str(x_date)
# 
# # Now do the same using vectors 
# yrs <- c(1999, 2012, 1980)
# mths <- c(1, 3, 11)
# dys <- c(28, 3, 5)
# 
# ymd(paste(yrs, ___, ___, sep = ___))
# 
# # What happens when there are data missing?
# ymd(paste(2017, NA, 15, sep = "-"))


