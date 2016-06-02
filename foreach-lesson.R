##' Robin Elahi
##' 16 April 2016
##' foreach
##' 
##' 


library(foreach)

# the default output of foreach is a list
x <- foreach(i = 1:3) %do% sqrt(i)
x

x <- foreach(a = 1:3, b = rep(10, 3)) %do% (a + b)
x

x <- foreach(a = 1:1000, b = rep(10, 2)) %do% {
  a + b
}
x

##### Using .combine #####

# Combining numeric output into a vector
x <- foreach(i = 1:3, .combine = 'c') %do% exp(i)
x

# Combining vectors into a matrixf
x <- foreach(i = 1:4, .combine = 'cbind') %do% rnorm(4)
x

# Can also use + or *
foreach(i = 1:4, .combine = '+') %do% rnorm(4)
foreach(i = 1:4, .combine = '*') %do% rnorm(4)

# Specify a user written function to combine results
cfun <- function(a, b) NULL
foreach(i = 1:4, .combine = 'cfun') %do% rnorm(4)

# If function allows many arguments, use .multicombine
cfun <- function(...) NULL
x <- foreach(i = 1:4, .combine = 'cfun', .multicombine = TRUE) %do% rnorm(4)
x
