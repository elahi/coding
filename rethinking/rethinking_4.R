##' Robin Elahi
##' 16 June 2017
##' Statistical Rethinking (McElreath)
##' Chapter 4

library(rethinking)
# library(dplyr)
# library(tibble)
# library(ggplot2)

##### R CODE SNIPPETS ######

# 4.1 - Normal by addition
pos <- replicate(1000, sum(runif(16, -1, 1)))
pos
dens(pos)

# 4.2-4.4 - Normal by multiplication
prod(1 + runif(12, 0, 0.1))

growth <- replicate(1e4, prod(1 + runif(12, 0, 0.1)))
dens(growth, norm.comp = TRUE)

big <- replicate(1e4, prod(1 + runif(12, 0, 0.5)))
small <- replicate(1e4, prod(1 + runif(12, 0, 0.01)))

dens(big, norm.comp = TRUE)
dens(small, norm.comp = TRUE)

# 4.5 - Normal by log multiplication
log_big <- replicate(1e4, log(prod(1 + runif(12, 0, 0.5))))
dens(log_big, norm.comp = TRUE)

dnorm(0, 0, 0.1)

# 4.6
w <- 6; n <- 9;
p_grid <- seq(from = 0, to = 1, length.out = 100)
posterior <- dbinom(w, n, p_grid) * dunif(p_grid, 0, 1)
posterior <- posterior/(sum(posterior))
plot(p_grid, posterior)

# 4.7-4.10 - Gaussian model of height
data("Howell1")
d <- Howell1
str(d)
d2 <- d[d$age >= 18, ]
dens(d2$height)

# 4.11
curve(dnorm(x, 178, 20), from = 100, to = 250)
curve(dunif(x, 0, 50), from = -10, to = 60)

sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 10)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h, norm.comp = TRUE)
