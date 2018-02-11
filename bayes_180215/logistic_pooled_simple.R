#################################################
# Author: Robin Elahi
# Date: 2018-02-15
# Intro to Bayes, using Adriatic interview data
#################################################

##### LOAD PACKAGES, DATA #####

setwd("bayes_180215/")

library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(bayesplot)

## Load rstan
library(rstan)
## As the startup message says, if you are using rstan locally on a multicore machine and have plenty of RAM to estimate your model in parallel, at this point execute
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# These options respectively allow you to automatically save a bare version of a compiled Stan program to the hard disk so that it does not need to be recompiled and to execute multiple Markov chains in parallel.

## Load data
dat <- read.csv("interview_logistic_subset.csv")
names(dat)

##### VISUALIZE DATA #####
dat %>% count(closure_effect)

## Effect of nation
dat %>% 
  ggplot(aes(Nation, fill = closure_effect)) + 
  geom_bar()

## Effect of distance to closure
dat %>%
  ggplot(aes(dist_pomo, closure_effect01, color = Nation)) +
  stat_smooth(mapping = aes(color = NULL), 
              method = "glm", method.args = list(family = "binomial"),
              se = TRUE, color = "black") +
  geom_jitter(alpha = 0.5, size = 2, width = 0, height = 0.05) +
  labs(x = "Distance to Jabuka-Pomo border (km)",
       y = "Probability",
       title = "Were you affected by the closure?") + 
  theme(legend.position = "bottom")

## Effect of vessel size 
dat %>%
  ggplot(aes(ves_length, closure_effect01, color = Nation)) +
  stat_smooth(mapping = aes(color = NULL), 
              method = "glm", method.args = list(family = "binomial"),
              se = TRUE, color = "black") +
  geom_jitter(alpha = 0.5, size = 2, width = 0, height = 0.05) +
  labs(x = "Vessel length (m)",
       y = "Probability",
       title = "Were you affected by the closure?") + 
  theme(legend.position = "bottom")

## I need to remove rows with NAs
stat_dat <- dat[complete.cases(dat), ]

#' I want to estimate the following model:
#' Pr(affected = 1) = intercept + nation + distance_to_closure + vessel_size
#' I have rescaled the variables as follows:
#' Affected: Yes = 1, No = 0
#' Nation: Croatia = 0, Italy = 1
#' Distance (km) and Vessel Size (m): (xi - mean(x)) / (2 * sd(x)) 
#' Gelman, Andrew. "Scaling regression inputs by dividing by two standard deviations." Statistics in medicine 27.15 (2008): 2865-2873.

##### GLM #####

glm1 <- glm(closure_effect01 ~ Nation01 +  dist_pomo_z + ves_length_z, 
            data = stat_dat, family = binomial(link = "logit"))
summary(glm1)
tidy(glm1)

##### STAN #####

## Options
n_warmup <- 1000
n_iter <- 2000
n_thin <- 1
n_chains <- 4
n_cores <- 4

## Create a list with the chosen variables
data_list <- list(
  N = nrow(stat_dat),
  y = stat_dat$closure_effect01, 
  x1 = stat_dat$Nation01, 
  x2 = stat_dat$dist_pomo_z, 
  x3 = stat_dat$ves_length_z
)
data_list

## Create a list of initial values
init_list <- list(
  list(alpha = 0.5, b1 = 0, b2 = 2, b3 = -2), 
  list(alpha = 2.5, b1 = 0.5, b2 = 5, b3 = 2), 
  list(alpha = -3.5, b1 = -2, b2 = 0, b3 = -4), 
  list(alpha = 5, b1 = 4, b2 = -3, b3 = 0)
)

## Stan model
sink("stan/logistic-pooled.stan")
cat("// logistic-pooled.stan
data {
  int<lower=0> N;             // number of observations
  int<lower=0,upper=1> y[N];  // set the (binary) dependent variable as bounded between 0 and 1
  vector[N] x1;               // Nation
  vector[N] x2;               // Distance to closure
  vector[N] x3;               // Vessel length
}
parameters {
  real alpha;
  real b1;
  real b2;
  real b3;
}
model {
  alpha ~ normal(0,10);       // prior on intercept
  b1 ~ normal(0,10);          // prior on coefficient
  b2 ~ normal(0,10);
  b3 ~ normal(0,10);
  y ~ bernoulli_logit(alpha + b1*x1 + b2*x2 + b3*x3);
}
", fill = TRUE)
sink()

## Estimate the model
stan1 <- stan(file = "stan/logistic-pooled.stan", data = data_list, init = init_list, 
              iter = n_iter, warmup = n_warmup, thin = n_thin, chains = n_chains, cores = n_cores)

class(stan1)
summary(stan1)
print(stan1, digits = 1)
plot(stan1, pars = c("alpha","b1", "b2", "b3"))
traceplot(stan1, pars = c("alpha","b1", "b2", "b3"))

## Compare with frequentist
tidy(stan1)
tidy(glm1)

## Compare with rstanarm
library(rstanarm)
my_seed <- 112
stan2 <- stan_glm(closure_effect01 ~ Nation01 +  dist_pomo_z + ves_length_z, 
                  data = stat_dat, 
                  family = binomial(link = "logit"), 
                  prior_intercept = normal(0, 10),
                  prior = normal(0, 10), 
                  QR = TRUE,
                  cores = n_cores, seed = my_seed)

tidy(stan2)
tidy(stan1, pars = c("alpha","b1", "b2", "b3"))
tidy(glm1)
plot(stan2)

##### BAYESPLOT: PLOTTING MCMC DRAWS #####

## Diagnostics and inference using Bayesplot
## http://mc-stan.org/bayesplot/articles/index.html

## Extract posterior draws
posterior <- as.array(stan1)
dim(posterior)
dimnames(posterior)

## http://mc-stan.org/bayesplot/articles/plotting-mcmc-draws.html

## Histograms
mcmc_hist(posterior, pars = c("alpha","b1", "b2", "b3"))

## Credible intervals
mcmc_intervals(posterior, pars = c("alpha","b1", "b2", "b3"))

## Posterior density curves
mcmc_areas(
  posterior,
  pars = c("alpha","b1", "b2", "b3"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)

## Densities by chain
mcmc_dens_overlay(posterior, pars = c("alpha","b1", "b2", "b3"))

## Trace plots
color_scheme_set("viridis")
mcmc_trace(posterior, pars = c("alpha","b1", "b2", "b3"))

##### BAYESPLOT: MCMC DIAGNOSTICS #####

## Rhats
## One way to monitor whether a chain has converged to the equilibrium distribution is to compare its behavior to other randomly initialized chains. This is the motivation for the Gelman and Rubin (1992) potential scale reduction statistic, R̂ . The R̂  statistic measures the ratio of the average variance of samples within each chain to the variance of the pooled samples across chains; if all chains are at equilibrium, these will be the same and R̂  will be one. If the chains have not converged to a common distribution, the R̂  statistic will be greater than one. (Stan Development Team, 2016).

print(stan1, digits_summary = 3)
rhats <- rhat(stan1, pars = c("alpha","b1", "b2", "b3"))
print(rhats)

## Effective sample size
## The effective sample size is an estimate of the number of independent draws from the posterior distribution of the estimand of interest. Because the draws within a Markov chain are not independent if there is autocorrelation, the effective sample size, neff, will be smaller than the total sample size, N. The larger the ratio of neff to N the better.

print(stan1, digits_summary = 3)
ratios_cp <- neff_ratio(stan1, pars = c("alpha","b1", "b2", "b3"))
print(ratios_cp)
