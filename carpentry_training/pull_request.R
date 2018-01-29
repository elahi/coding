
surveys <- read.csv("data/portal_data_joined.csv")
library(lubridate)

paste(surveys$year, surveys$month, surveys$day, sep = "-")

my_date <- ymd("2015-01-01")
str(my_date)

my_date <- ymd(paste("2015", "1", "1", sep = "-"))
str(my_date)

# What happens when we try to define an invalid date?
ymd("2017-02-30", quiet = T) # no warning, even though quiet is TRUE
?ymd

# Now let's create a date in our surveys dataset
surveys$date <- ymd(paste(surveys$year, surveys$month, surveys$day, sep = "-"))
str(surveys)

# Check for NAs 
summary(surveys$date) # yes, some dates failed to parse

# Create a character vector of our dates
surveys_dates <- paste(surveys$year, surveys$month, surveys$day, sep = "-")

surveys_dates[is.na(surveys$date)]

# Why did these dates fail to parse?

# 
# Please delete the text below before submitting your contribution. 
# 
# ---
#   
#   Thanks for contributing! If this contribution is for instructor training, please send an email to checkout@carpentries.org with a link to this contribution so we can record your progress. You’ve completed your contribution step for instructor checkout just by submitting this contribution.  
# 
# Please keep in mind that lesson maintainers are volunteers and it may be some time before they can respond to your contribution. Although not all contributions can be incorporated into the lesson materials, we appreciate your time and effort to improve the curriculum.  If you have any questions about the lesson maintenance process or would like to volunteer your time as a contribution reviewer, please contact Kate Hertweck (k8hertweck@gmail.com).  
# 
# ---
#   
