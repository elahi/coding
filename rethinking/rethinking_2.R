##' Robin Elahi
##' 10 May 2017
##' Statistical Rethinking (McElreath)
##' Chapter 2

library(rethinking)

##### R CODE SNIPPETS ######

## 2.1
ways <- c(0, 3, 8, 9, 0)
ways/sum(ways)

## 2.2
dbinom(6, size = 9, prob = 0.5)

## 2.3
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)

# define prior
prior <- rep(1, 20)

# compute likelihood at each value in grid
likelihood <- dbinom(6, size = 9, prob = p_grid)

# compute produce of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
posterior

## 2.4
plot(p_grid, posterior, type = "b", xlab = "probability of water", 
     ylab = "posterior probability")
mtext("20 points")

## 2.5.1
# define prior
prior <- ifelse(p_grid < 0.5, 0, 1)
# compute likelihood at each value in grid
likelihood <- dbinom(6, size = 9, prob = p_grid)
# compute produce of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
# plot
plot(p_grid, posterior, type = "b", xlab = "probability of water", 
     ylab = "posterior probability")
mtext("20 points")

## 2.5.2
# define prior
prior <- exp(-5 * abs(p_grid - 0.5))
# compute likelihood at each value in grid
likelihood <- dbinom(6, size = 9, prob = p_grid)
# compute produce of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
# plot
plot(p_grid, posterior, type = "b", xlab = "probability of water", 
     ylab = "posterior probability")
mtext("20 points")
plot(p_grid, prior, type = "b", xlab = "probability of water", 
     ylab = "prior probability")
mtext("20 points")

## 2.6 - quadratic approximation of globe data
globe.qa <- map(
  alist(
    w ~ dbinom(9, p), # binomial likelihood
    p ~ dunif(0, 1)   # uniform prior
  ), 
  data = list(w = 6))

# display summary of quadratic approximation
precis(globe.qa)

## 2.7
# analytical calculation
w <- 6
n <- 9
curve(dbeta(x, w+1, n-w+1), from = 0, to = 1)
# quadratic approximation
curve(dnorm(x, 0.67, 0.16), lty = 2, add = TRUE)

##### PRACTICE - 2M1 ######

## 2M1
##' Compute and plot the grid approximate posterior distribution for each of the following:
##' (1) W W W
##' (2) W W W L
##' (3) L W W L W W W 

##' (1) W W W
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 50)
# define prior
prior <- rep(1, 50)
# compute likelihood at each value in grid
likelihood <- dbinom(3, size = 3, prob = p_grid)
# compute produce of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
# plot
plot(p_grid, posterior, type = "b", xlab = "probability of water", 
     ylab = "posterior probability")

##' (1) W W W L 
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 50)
# define prior
prior <- rep(1, 50)
# compute likelihood at each value in grid
likelihood <- dbinom(3, size = 4, prob = p_grid) # NOTE THAT THIS CHANGED
# compute produce of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
# plot
plot(p_grid, posterior, type = "b", xlab = "probability of water", 
     ylab = "posterior probability")

##' (3) L W W L W W W 
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 50)
# define prior
prior <- rep(1, 50)
# compute likelihood at each value in grid
likelihood <- dbinom(5, size = 7, prob = p_grid) # NOTE THAT THIS CHANGED
# compute produce of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
# plot
plot(p_grid, posterior, type = "b", xlab = "probability of water", 
     ylab = "posterior probability")

##### PRACTICE - 2M2 ######
## 2M2
##' Now assume a prior for p that is equal to zero when p < 0.5 and is a positive constant when  P > 0.5

##' (1) W W W
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 50)
# define prior
prior <- ifelse(p_grid < 0.5, 0, 1)
# compute likelihood at each value in grid
likelihood <- dbinom(3, size = 3, prob = p_grid)
# compute produce of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
# plot
plot(p_grid, posterior, type = "b", xlab = "probability of water", 
     ylab = "posterior probability")

##' (1) W W W L 
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 50)
# define prior
prior <- ifelse(p_grid < 0.5, 0, 1)
# compute likelihood at each value in grid
likelihood <- dbinom(3, size = 4, prob = p_grid) # NOTE THAT THIS CHANGED
# compute produce of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
# plot
plot(p_grid, posterior, type = "b", xlab = "probability of water", 
     ylab = "posterior probability")

##' (3) L W W L W W W 
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 50)
# define prior
prior <- ifelse(p_grid < 0.5, 0, 1)
# compute likelihood at each value in grid
likelihood <- dbinom(5, size = 7, prob = p_grid) # NOTE THAT THIS CHANGED
# compute produce of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
# plot
plot(p_grid, posterior, type = "b", xlab = "probability of water", 
     ylab = "posterior probability")

##### PRACTICE - 2M3 ######
## 2M3
##' Two globes, one for Earth and one for Mars
##' Earth globe is 70% covered in water
##' Mars globe is 100% land
##' One of these globes was tossed in the air produced a 'land' observation
##' Assume that each globe was likely to be tossed
##' Show that the posterior probability that the globe was Earth, conditional on seeing 'land', is 0.23
##' Pr(Earth|land) = 0.23

p_E = 1/2 # probability that globe is Earth
p_M = 1/2 # probability that globe is Mars

p_E_land = 0.23 # probability that the globe is Earth, given land

p_land_E = 0.3 # probability of land, given Earth
p_land_M = 1 # probability of land, given Mars

## Bayes Theorem
## Pr(Earth | land) = Pr(land | Earth) Pr(Earth) / Pr(land)

## One remaining piece to solve for left hand side
p_land = p_land_E * p_E + p_land_M * p_M

## Solve
p_land_E * p_E / p_land

##### PRACTICE - 2M4 ######

##' Deck with three cards, each has 2 sides, and each side is either black or white
##' 
