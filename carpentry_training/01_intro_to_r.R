##' Robin Elahi
##' 2 Jan 2018
##' 16 Apr 2018
##' R-Ecology-Lesson
##' 01-intro-to-r

##### OBJECTIVES #####

# Define the following terms as they relate to R: object, assign, call, function, arguments, options.
# Assign values to variables in R.
# Learn how to name objects
# Use comments to inform script.
# Solve simple arithmetic operations in R.
# Call functions and use arguments to change their default options.
# Inspect the content of vectors and manipulate their content.
# Subset and extract values from vectors.
# Analyze vectors with missing data.

##### Creating objects #####

3 + 5
12 / 7

weight_kg <- 55

##' <- 
##' pc: Alt + -
##' mac: option + -, or control + -
##' 

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

# Types this, then ask - 
weight_kg <- 100
# What do you think is the current content of the object weight_lb? 126.5 or 220?

### Comments
#' comment shortcut
#' Ctrl + Shift + C

### Challenge 1
## What are the values after each statement in the following?
mass <- 45            # mass?
age <- 120            # age?
mass <- mass * 2.0    # mass?
age <-age-20          # age?
mass_index <- mass/age # mass_index?

### Functions

#' Function = canned script that automates more complicated sets of commands
#' argument = input in a function
#' value = returned by a function
#' calling = executing a function ('running it')
#' options = argument that has a default value, but that can be changed

round(3.14159)
args(round)
round(3.15151, digits = 2)

## if you provide the args you don't have to name them
## objects vs variables (we use them interchangeably here)

### Vectors and data types
#' Vectors can be numbers or characters
#' atomic vector is the simplest R data type

# Numeric vector
weight_g <- c(50, 60, 65, 82)
weight_g

# Character vector
animals <- c("mouse", "rat", "dog")
animals

length(weight_g)
length(animals)

class(weight_g)
class(animals)

str(weight_g)
str(animals)

## Use c() to add to the vector
weight_g <- c(weight_g, 90)
weight_g <- c(30, weight_g)
typeof(weight_g)

## Other vector data types
# logical (TRUE or FALSE)
# integer (e.g., 2L)

## Other data structures, besides vectors
# lists, matrices, data frames, factors, arrays

### Challenge 2 

## We’ve seen that atomic vectors can be of type character, numeric (or double), integer, and logical. But what happens if we try to mix these types in a single vector?
# R implicitly converts them all to be the same type

## What will happen in each of these examples?
# hint(use class() to check the data type)
num_char <- c(1, 2, 3, "a")
num_logical <- c(1, 2, 3, TRUE)
char_logical <- c("a", "b", "c", TRUE)
tricky <- c(1, 2, 3, "4")

num_logical
typeof(char_logical)
class(char_logical)

## Why do you think it happens?
# Vectors can only be of one type. R converts (coerces) the content to find a 'common denominator'

## How many values in combined_logical are "TRUE" as a character in the following example?
num_logical <- c(1, 2, 3, TRUE)
char_logical <- c("a", "b", "c", TRUE)
combined_logical <- c(num_logical, char_logical)
# Only one. The 'TRUE' in num_logical gets converted into a 1 before it gets converted to "1" in combined_logical

## You’ve probably noticed that objects of different types get converted into a single, shared type within a vector. In R, we call converting objects from one class into another class coercion. These conversions happen according to a hierarchy, whereby some types get preferentially coerced into other types. Can you draw a diagram that represents the hierarchy of how these data types are coerced?
# log > num > char < log

##### Subsetting vectors #####
animals <- c("mouse", "rat", "dog", "cat")
animals[2]
animals[c(3,2)]

more_animals <- animals[c(1,2,3,2,1,4)]
more_animals

## Conditional subsetting
weight_g <- c(21, 23, 34, 39, 54)
weight_g[c(TRUE, FALSE, TRUE, FALSE, FALSE)] # Usually we don't type these out by hand

## Instead, use output from a logical test
weight_g > 50

weight_g[weight_g > 50]

## Combining multiple tests
weight_g[weight_g < 30 | weight_g > 50]
weight_g[weight_g >= 30 & weight_g == 21]

## Function %in%
animals <- c("mouse", "rat", "dog", "cat")
animals[animals == "cat" | animals == "rat"]
animals %in% c("rat", "cat", "dog", "duck", "goat")

### Challenge 3 
## Can you figure out why:
"four" >  "five" 
# alphabetical
"fox" > "fart"

##### Missing data #####

### Missing data
heights <- c(2, 4, 4, NA, 6)

mean(heights)
max(heights)
mean(heights, na.rm = TRUE)
max(heights, na.rm = TRUE)

## is.na()
## na.omit()
## complete.cases()

## Extract those elements which are not missing values.
heights[!is.na(heights)]

## Returns the object with incomplete cases removed. The returned object is an atomic vector of type `"numeric"` (or `"double"`).
na.omit(heights)

## Extract those elements which are complete cases. The returned object is an atomic vector of type `"numeric"` (or `"double"`).
heights[complete.cases(heights)]

### Challenge 4

## Using this vector of length measurements, create a new vector with the NAs removed.
lengths <- c(10, 24, NA, 18, NA, 20)

# Two options
lengths[!is.na(lengths)]
na.omit(lengths)

## Use the function median() to calculate the median of the lengths vector
median(lengths, na.rm = T)


