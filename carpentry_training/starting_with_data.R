##' Robin Elahi
##' 9 Nov 2017
##' http://www.datacarpentry.org/R-ecology-lesson/02-starting-with-data.html


##### PRESENTATION OF THE SURVEY DATA #####

# download.file("https://ndownloader.figshare.com/files/2292169",
#               "data/portal_data_joined.csv")
#               
surveys <- read.csv("data/portal_data_joined.csv")

##### WHAT ARE DATAFRAMES? #####

head(surveys)
str(surveys)
dim(surveys)
nrow(surveys)
ncol(surveys)
names(surveys)
rownames(surveys)

## Challenge
#' What is the class - data.frame
#' Rows = 34786
#' Columns = 13
#' Species = 40

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

##' Challenge
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

## bar plot of the number of females and males captured during the experiment:
plot(surveys$sex)


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

# Fixed this in code
surveys_dates <- ymd(paste(surveys$year, surveys$month, surveys$day, sep = "-"))
head(surveys_dates)

# Typo
surveys_dates[is.na(surveys$date)]
# Maybe this:
is.na(surveys_dates)
surveys_dates[is.na(surveys_dates)]
str(surveys_dates)
unique(surveys_dates)

# Or this
surveys_dates[is.na(surveys$day)]
unique(surveys$day)
surveys_dates[is.na(surveys$year)]
surveys_dates[is.na(surveys$month)]



