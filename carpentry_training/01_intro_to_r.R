##' Robin Elahi
##' 2 Jan 2017
##' R-Ecology-Lesson

##### 01-intro-to-r #####

### Creating objects in R
3 + 5
12 / 7

weight_kg <- 55

##' <- 
##' mac: Alt + -
##' pc: option + -

##' Object names should be:
##' explicit
##' not too long
##' cannot start with a number
##' case sensitive
##' don't overwrite other function names
##' avoid dots (.)
##' use nouns for variable names, and verbs for function names
##' be consistent with spacing
##' consistent coding style

weight_kg <- 55    # doesn't print anything
(weight_kg <- 55)  # but putting parenthesis around the call prints the value of `weight_kg`
weight_kg          # and so does typing the name of the object

2.2 * weight_kg

weight_kg <- 57.5

2.2 * weight_kg

weight_lb <- 2.2 * weight_kg

weight_kg <- 100

### Comments

#' comment shortcut
#' Ctrl + Shfit + C

### Functions

#' Function = canned script that automates more complicated sets of commands
#' argument = input in a function
#' value = returned by a function
#' calling = executing a function ('running it')
#' options = argument that has a default value, but that can be changed

round(3.14159)
args(round)
round(3.15151, digits = 2)

# if you provide the args you don't have to name them

## objects vs variables

### Vectors and data types

#' Vectors can be numbers or characters
#' atomic vector is the simplest R data type


typeof(c("red",2))
typeof(c(2L,2L))

num_char <- c(1, 2, 3, "a")
num_logical <- c(1, 2, 3, TRUE)
num_logical
char_logical <- c("a", "b", "c", TRUE)
typeof(char_logical)
class(char_logical)

tricky <- c(1, 2, 3, "4")

"four" >  "five"

### Missing data
heights <- c(2, 4, 4, NA, 6)

## Extract those elements which are not missing values.
heights[!is.na(heights)]

## Returns the object with incomplete cases removed. The returned object is an atomic vector of type `"numeric"` (or `"double"`).
na.omit(heights)

## Extract those elements which are complete cases. The returned object is an atomic vector of type `"numeric"` (or `"double"`).
heights[complete.cases(heights)]

## Using this vector of length measurements, create a new vector with the NAs removed.
lengths <- c(10, 24, NA, 18, NA, 20)
lengths[!is.na(lengths)]

median(lengths, na.rm = T)

