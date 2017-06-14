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

# 3.20
# 2 globe tosses; three possible observations
dbinom(0:2, size = 2, prob = 0.7)

# 3.21-3.24 simulations
rbinom(1, size = 2, prob = 0.7)
rbinom(10, size = 2, prob = 0.7)
dummy_w <- rbinom(1e5, size = 2, prob = 0.7)
table(dummy_w)/1e5
simplehist(dummy_w, xlab = "dummy water count")

dummy_w <- rbinom(1e5, size = 9, prob = 0.7)
simplehist(dummy_w, xlab = "dummy water count")

# 3.25
w <- rbinom(1e4, size = 9, prob = 0.6)
simplehist(w)

# 3.26
# propagate paramter uncertainty by using samples from the posterior
samples
hist(samples)
w <- rbinom(1e4, size = 9, prob = samples)
simplehist(w)

##### PRACTICE ######
dbinom(x = 1, size = 3, prob = 0.7)
rbinom(n = 1e4, size = 3, prob = 0.7)

# Code for easy practice set
p_grid <- seq(from = 0, to = 1, length.out = 1000)
plot(p_grid)
prior <- rep(1, 1000)
plot(prior)
likelihood <- dbinom(6, size = 9, prob = p_grid)
plot(likelihood)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
plot(p_grid, posterior)
set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
plot(samples)

## Use the values in samples to answer the following questions:
# 3E1 - how much posterior probability lies below p = 0.2?
plot(p_grid, posterior)
sum(posterior)
sum(posterior[p_grid < 0.2])

# 3E2 - how much posterior probability lies above p = 0.8?
sum(posterior[p_grid > 0.8])

# 3E3 - how much pp lies between p = 0.2 and 0.8?
sum(posterior[p_grid > 0.2 & p_grid < 0.8])

# 3E4 - 20% of pp lies below which value of p?
quantile(samples, 0.2)

# 3E5 - 20% of pp lies above which value of p?
quantile(samples, 0.8)

# 3E6 - which values of p contain the narrowest interval equal to 66% of the pp?
HPDI(samples, prob = 0.66)

# 3E7 - which values of p contain 66% of the pp, assuming equal posterior probability both below and above the interval?
PI(samples, prob = 0.66)

# 3M1
## 8 water, 15 tosses. Construct posterior, using grid approximation. Use same flat prior. 
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
plot(p_grid, posterior)

# 3M2
set.seed(100) 
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
HPDI(samples, prob = 0.9)

# 3M3
## Construct a posterior predicive check for this model and data. Simulate the distribution of samples, averaging over the posterior uncertainty in p. What is the probability of observing 8 water in 15 samples?
w <- rbinom(1e4, size = 15, prob = samples)
plot(w)
simplehist(w)
length(which(w == 8))
sum(w == 8)/length(w)
mean(w == 8)

# 3M4
## Using the posterior distribution constructed from the new data (8/15), now calculate the prob of observing 6/9
w <- rbinom(1e4, size = 9, prob = samples)
simplehist(w)
mean(w == 6)

# 3M5
## Start at M1, but use a prior that is zero below p = 0.5 and a constant above p = 0.5
## Repeat each problem abov and compare inferences. 
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- ifelse(p_grid < 0.5, 0, 1)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
plot(p_grid, posterior)

set.seed(100) 
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
HPDI(samples, prob = 0.9)

w <- rbinom(1e4, size = 15, prob = samples)
plot(w)
simplehist(w)
length(which(w == 8))
sum(w == 8)/length(w)
mean(w == 8)

w <- rbinom(1e4, size = 9, prob = samples)
simplehist(w)
mean(w == 6)
