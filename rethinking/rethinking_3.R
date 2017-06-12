##' Robin Elahi
##' 12 June 2017
##' Statistical Rethinking (McElreath)
##' Chapter 3

library(rethinking)

##### R CODE SNIPPETS ######

## 3.1
## Illustrating the consequences of false positives whenever a condition is rare

PrPV <- 0.95 # prob that the test is positive, given a vampire [true positive]
PrPM <- 0.01 # prob that the test is positive, given a mortal [false positive]
PrV <- 0.001 # prob that an individual is a vampire
PrP <- PrPV * PrV + PrPM * (1 - PrV) # average prob that a test is positive [denom in Bayes theorem]

(PrVP <- PrPV * PrV / PrP) # Bayes theorem

## 3.2
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
posterior

## 3.3
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

## 3.4
plot(samples)

## 3.5
dens(samples)

## 3.6
# add up posterior probability where p < 0.5
sum(posterior[p_grid < 0.5])

# 3.7
sum(samples < 0.5) / 1e4

# 3.8
sum(samples > 0.5 & samples < 0.75) / 1e4

# 3.9
quantile(samples, 0.8)

# 3.10
quantile(samples, c(0.1, 0.9))

# 3.11
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(3, size = 3, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
plot(samples)

# 3.12
PI(samples, prob = 0.5)

# 3.13
HPDI(samples, prob = 0.5 )

##' If the chose of interval type makes a big differece, then you shouldn't be using intervals to summarise the posterior
##' 

# 3.14-3.16
p_grid[which.max(posterior)]
chainmode(samples, adj = 0.01)
mean(samples)
median(samples)

# 3.17-3.19
sum(posterior * abs(0.5 - p_grid))
loss <- sapply(p_grid, function(d) sum(posterior * abs(d - p_grid)))
loss     
p_grid[which.min(loss)]
plot(loss)
median(samples)

##' absolute loss leads to the median as the point estimate
##' quadratic loss [(d - p)^2] leads the posterior mean as the point estimate
##' the details of the applied context may demand a rather unique loss function
