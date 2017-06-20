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

# 4.11-4.13
curve(dnorm(x, 178, 20), from = 100, to = 250)
curve(dunif(x, 0, 50), from = -10, to = 60)

sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 10)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h, norm.comp = TRUE)

# 4.14 - Grid approximation of the posterior distribution
mu.list <- seq(from = 140, to = 160, length.out = 200)
sigma.list <- seq(from = 4, to = 9, length.out = 200)
post <- expand.grid(mu = mu.list, sigma = sigma.list)
post$LL <- sapply(1:nrow(post), 
                  function(i) sum(dnorm(
                    d2$height, 
                    mean = post$mu[i], 
                    sd = post$sigma[i], 
                    log = TRUE)))

post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) + dunif(post$sigma, 0, 50, TRUE)

post$prob <- exp(post$prod - max(post$prod))

head(post)

# 4.15-4.16 - plotting the posterior
contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob)

# 4.17-4.20 - sampling from the posterior
sample.rows <- sample(1:nrow(post), size = 1e4, replace = TRUE, prob = post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

plot(sample.mu, sample.sigma, cex = 1, pch = 16, col = col.alpha(rangi2, 0.1))

## Marginal posterior densities of mu and sigma
dens(sample.mu)
dens(sample.sigma)

## HPDI of mu and sigma
HPDI(sample.mu)
HPDI(sample.sigma)

# 4.21 - How does sample size affect the distribution of sigma?
d3 <- sample(d2$height, size = 20)

mu.list <- seq(from = 150, to = 170, length.out = 200)
sigma.list <- seq(from = 4, to = 20, length.out = 200)
post2 <- expand.grid(mu = mu.list, sigma = sigma.list)
post2$LL <- sapply(1:nrow(post2), function(i) 
  sum(dnorm(d3, mean = post2$mu[i], sd = post2$sigma[i], log = TRUE)))

post2$prod <- post2$LL + dnorm(post2$mu, 178, 20, TRUE) + dunif(post2$sigma, 0, 50, TRUE)
post2$prob <- exp(post2$prod - max(post2$prod))
sample2.rows <- sample(1:nrow(post2), size = 1e4, replace = TRUE, prob = post2$prob)
sample2.mu <- post2$mu[sample2.rows]
sample2.sigma <- post2$sigma[sample2.rows]
plot(sample2.mu, sample2.sigma, cex = 1, pch = 16, col = col.alpha(rangi2, 0.1))
dens(sample2.sigma, norm.comp = TRUE)

# 4.24 - fitting the model with MAP
