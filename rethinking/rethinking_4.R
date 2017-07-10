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

# 4.24-4.27 - fitting the model with MAP
library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18, ]

flist <- alist(height ~ dnorm(mu, sigma), 
               mu ~ dnorm(178, 20), 
               sigma ~ dunif(0, 50))

m4.1 <- map(flist, data = d2)

precis(m4.1)

# 4.28 start values for map
start <- list(mu = mean(d2$height), 
              sigma = sd(d2$height))

start

# 4.29 stronger prior
m4.2 <- map(
  alist(
    height ~ dnorm(mu, sigma), 
    mu ~ dnorm(178, 0.1), 
    sigma ~ dunif(0, 50)
  ), 
  data = d2)

precis(m4.2)

# 4.30-4.31 vcov
## variance is the square of sigma (sd)
vcov(m4.1)
vcov(m4.2)

diag(vcov(m4.1))

cov2cor(vcov(m4.1))

# 4.32-4.33 extracting samples from multi-dimensional posterior
post <- extract.samples(m4.1, n = 1e4)
head(post)
precis(post)
precis(m4.1)
plot(post)

# 4.34 - under the hood w/multivariate sampling
library(MASS)
post <- mvrnorm(n = 1e4, mu = coef(m4.1), Sigma = vcov(m4.1))
coef(m4.1)
vcov(m4.1)

# 4.35-4.37 - getting sigma right
m4.1_logsigma <- map(
  alist(
    height ~ dnorm(mu, exp(log_sigma)), 
    mu ~ dnorm(178, 20), 
    log_sigma ~ dnorm(2, 10)
  ), 
  data = d2)
m4.1_logsigma

post <- extract.samples(m4.1_logsigma)
sigma <- exp(post$log_sigma)
plot(sigma)
plot(post$log_sigma)
dens(sigma)
dens(post)

# 4.37 - adding a predictor
plot(height ~ weight, data = d2)

# 4.38
library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18, ]

# fit model
m4.3 <- map(
  alist(
    height ~ dnorm(mu, sigma), 
    mu <- a + b*weight, 
    a ~ dnorm(156, 100), 
    b ~ dnorm(0, 10), 
    sigma ~ dunif(0, 50)
  ), 
  data = d2)

precis(m4.3)
precis(m4.3, corr = T) # note the correlation between a and b

# Center weight
d2$weight.c <- d2$weight - mean(d2$weight)
m4.4 <- map(
  alist(
    height ~ dnorm(mu, sigma), 
    mu <- a + b*weight.c, 
    a ~ dnorm(156, 100), 
    b ~ dnorm(0, 10), 
    sigma ~ dunif(0, 50)
  ), 
  data = d2)

precis(m4.4)
precis(m4.4, corr = T)

plot(height ~ weight, data = d2)
abline(a = coef(m4.3)["a"], b = coef(m4.3)["b"])

post <- extract.samples(m4.3)
post[1:5, ]

N <- 10
dN <- d2[1:N, ]
mN <- map(
  alist(
    height ~ dnorm(mu, sigma), 
    mu <- a + b*weight, 
    a ~ dnorm(178, 100), 
    b ~ dnorm(0, 10), 
    sigma ~ dunif(0, 50)
  ), 
  data = dN)

# 4.49 - plot 20 of these lines
post <- extract.samples(mN, n = 20)
plot(dN$weight, dN$height, 
     xlim = range(d2$weight), ylim = range(d2$height), 
     col = rangi2, xlab = 'weight', ylab = 'height')
mtext(concat("N = ", N))
for(i in 1:20) abline(a = post$a[i], b = post$b[i], col = col.alpha("black", 0.3))

# 4.50
mu_at_50 <- post$a + post$b * 50
dens(mu_at_50, col = rangi2, lwd = 2, xlab = "mu|weight = 50")
HPDI(mu_at_50, prob = 0.89)

mu <- link(m4.3)
mu

# 4.54
# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
weight.seq <- seq(from = 25, to = 70, by = 1)
weight.seq
# use link to compute mu
# for each sample from the posterior
# and for each weight in weight.seq
mu <- link(m4.3, data = data.frame(weight = weight.seq))
str(mu)

# use type = "n" to hide raw data
plot(height ~ weight, d2, type = "n")
# loop over samples and plot each mu value
for(i in 1:100) points(weight.seq, mu[i, ], pch = 16, col = col.alpha(rangi2, 0.1))

# summarise the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.89)
mu.mean
mu.HPDI

# 4.57
# plot raw data
# fading out points to make line and interval more visible
plot(height ~ weight, data = d2, col = col.alpha(rangi2, 0.5))
# plot the map line, aka the mean mu for each weight
lines(weight.seq, mu.mean)
# plot a shaded region for HPDI
shade(mu.HPDI, weight.seq)

# 4.59
sim.height <- sim(m4.3, data = list(weight = weight.seq), n = 1e4)
str(sim.height)
height.PI <- apply(sim.height, 2, PI, prob = 0.95)
height.PI

# plot raw data
par(mfrow = c(1,1))
plot(height ~ weight, d2, col = col.alpha(rangi2, 0.5))
# draw map line
lines(weight.seq, mu.mean)
# draw HPDI region for line
shade(mu.HPDI, weight.seq)
# draw PI region for simulated heights
shade(height.PI, weight.seq)

# 4.63
post <- extract.samples(m4.3)
head(post)
weight.seq <- 25:70
sim.height <- sapply(weight.seq, function(weight)
  rnorm(
    n = nrow(post), 
    mean = post$a + post$b * weight, 
    sd = post$sigma
  ))

height.PI <- apply(sim.height, 2, PI, prob = 0.89)

# 4.64 - polynomial regression
head(d)
plot(height ~ weight, data = d)

# scale weight
d$weight.s <- (d$weight - mean(d$weight))/sd(d$weight)
plot(height ~ weight.s, data = d)

# get weight squared
d$weight.s2 <- d$weight.s^2

m4.5 <- map(
  alist(
    height ~ dnorm(mu, sigma), 
    mu <- a + b1* weight.s + b2*weight.s2, 
    a ~ dnorm(178, 100), 
    b1 ~ dnorm(0, 10), 
    b2 ~ dnorm(0, 10), 
    sigma ~ dunif(0, 10)
  ), 
  data = d
)

precis(m4.5)

weight.seq <- seq(from = -2.2, to = 2, length.out = 30)
pred_dat <- list(weight.s = weight.seq, weight.s2 = weight.seq^2)
mu <- link(m4.5, data = pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)
sim.height <- sim(m4.5, data = pred_dat)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)

plot(height ~ weight.s, d, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)

# 4.71 rescaling the axis
plot(height ~ weight.s, d, xaxt = "n")
at <- c(-2, -1, 0, 1, 2)
labels <- at * sd(d$weight) + mean(d$weight)
axis(side = 1, at = at, labels = round(labels, 1))

##### PRACTICE #####

### 4M1
# Simulate observed heights from the prior
sample_mu <- rnorm(1e4, 0, 10)
sample_sigma <- runif(1e4, 0, 10)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

### 4M2
flist <- alist(
  y ~ dnorm(mu, sigma), 
  mu <- dnorm(0, 10), 
  sigma <- dunif(0, 10)
)

### 4H1
# Make predictions for the following values:
weight.seq <- c(46.95, 43.72, 64.78, 32.59, 54.63)

# Use the original model
m4.3
precis(m4.3)

# Expected height:
mu_h1 <- link(m4.3, data = data.frame(weight = weight.seq))
head(mu_h1)

mu.link <- function(a, b, weight) mu = a + b * weight
a = 113.88
b = 0.9
predictions <- mu.link(a = a, b = b, weight = weight.seq)

# Summarise the posterior distribution:
mu.mean <- apply(mu_h1, 2, mean)
mu.HPDI <- apply(mu_h1, 2, HPDI, prob = 0.89)

predictions
mu.mean
mu.HPDI

### 4H2
dim(d)
dim(d2)
dj <- d[d$age < 18, ]

# fit model
m4h2 <- map(
  alist(
    height ~ dnorm(mu, sigma), 
    mu <- a + b*weight, 
    a ~ dnorm(156, 100), 
    b ~ dnorm(0, 10), 
    sigma ~ dunif(0, 50)
  ), 
  data = dj)

precis(m4h2)

# plot raw data
plot(height ~ weight, data = dj)
abline(a = coef(m4h2)["a"], b = coef(m4h2)["b"], col = 'red')

weight_max <- max(dj$weight)
weight_min <- min(dj$weight)

# make predictions for mean height
weight.seq <- seq(weight_min, weight_max, length.out = 100)
mu <- link(m4h2, data = data.frame(weight = weight.seq))
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.89)

lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)

# simulating heights
sim.height <- sim(m4h2, data = list(weight = weight.seq))
str(sim.height)

height.PI <- apply(sim.height, 2, PI, prob = 0.89)

## plot 
shade(height.PI, weight.seq)

### 4H3
d$weight.log <- log(d$weight)

m4h3 <- map(
  alist(
    height ~ dnorm(mu, sigma), 
    mu <- a + b * log(weight), 
    a ~ dnorm(178, 100), 
    b ~ dnorm(0, 100), 
    sigma ~ dunif(0, 50)
  ), 
  data = d)

precis(m4h3)

## Plot
plot(height ~ weight, data = d, col = col.alpha(rangi2, 0.4) )
plot(height ~ weight.log, data = d, col = col.alpha(rangi2, 0.4) )


weight_max <- max(d$weight)
weight_min <- min(d$weight)

# make predictions for mean height
weight.seq <- seq(weight_min, weight_max, length.out = 100)
mu <- link(m4h3, data = data.frame(weight = weight.seq))
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.97)

plot(height ~ weight, data = d, col = col.alpha(rangi2, 0.4) )
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)

# simulating heights
sim.height <- sim(m4h3, data = list(weight = weight.seq))
str(sim.height)

height.PI <- apply(sim.height, 2, PI, prob = 0.97)

## plot 
shade(height.PI, weight.seq)
