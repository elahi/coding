#################################################
# Author: Robin Elahi
# Date: 2018-02-15
# Intro to Bayes, using Adriatic interview data
#################################################

##### LOAD PACKAGES, DATA #####

setwd("bayes_180215/")

library(lme4)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(broom)
library(bayesplot)

## Load rstan
library(rstan)
## As the startup message says, if you are using rstan locally on a multicore machine and have plenty of RAM to estimate your model in parallel, at this point execute
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# These options respectively allow you to automatically save a bare version of a compiled Stan program to the hard disk so that it does not need to be recompiled and to execute multiple Markov chains in parallel.

dat <- read.csv("interview_logistic_subset.csv")
names(dat)

##### VISUALIZE DATA #####
dat %>% count(closure_effect)

## Effect of nation
dat %>%
  ggplot(aes(Nation, closure_effect01, color = Nation)) +
  geom_jitter(alpha = 0.5, size = 2, width = 0.2, height = 0.05) +
  labs(x = "Distance to Jabuka-Pomo border (km)",
       y = "Probability",
       title = "Nation") + 
  theme(legend.position = "none")

## Effect of distance to closure
dat %>%
  ggplot(aes(dist_pomo, closure_effect01, color = Nation)) +
  stat_smooth(mapping = aes(color = NULL), 
              method="glm", method.args=list(family="binomial"),
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
              method="glm", method.args=list(family="binomial"),
              se = TRUE, color = "black") +
  geom_jitter(alpha = 0.5, size = 2, width = 0, height = 0.05) +
  labs(x = "Vesel length (m)",
       y = "Probability",
       title = "Were you affected by the closure?") + 
  theme(legend.position = "bottom")

## I need to remove rows with NAs
stat_dat <- dat[complete.cases(dat), ]
## Create observation id
stat_dat <- stat_dat %>% mutate(obs_id = seq(1:n()))

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
max_treedepth <- 10

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
generated quantities {
  vector[N] y_new;            // Draws from posterior predictive distribution

  for(n in 1:N) {
    // Draw from ppd
    //y_new[n] = inv_logit(alpha + b1*x1[n] + b2*x2[n] + b3*x3[n]); // returns probability
    y_new[n] = bernoulli_rng(inv_logit(alpha + b1*x1[n] + b2*x2[n] + b3*x3[n])); // returns 0 or 1
  }
}", fill = TRUE)
sink()

## Estimate the model
stan1 <- stan(file = "stan/logistic-pooled.stan", data = data_list, init = init_list, 
              iter = n_iter, warmup = n_warmup, thin = n_thin, chains = n_chains, cores = n_cores)

class(stan1)
summary(stan1)
print(stan1, digits = 1)
plot(stan1)

## Compare with frequentist
tidy(stan1)
tidy(glm1)

##### DIAGNOSTICS #####

check_divergences(stan1)

mack_diagnostics <- get_sampler_params(stan1)

mack_diagnostics <- rstan::get_sampler_params(stan1) %>% 
  set_names(1:n_chains) %>% 
  map_df(as_data_frame,.id = 'chain') %>% 
  group_by(chain) %>% 
  mutate(iteration = 1:length(chain)) %>% 
  mutate(warmup = iteration <= n_warmup)

mack_diagnostics %>% 
  group_by(warmup, chain) %>% 
  summarise(percent_divergent = mean(divergent__ >0)) %>% 
  ggplot() +
  geom_col(aes(chain, percent_divergent, fill = warmup), position = 'dodge', color = 'black') + 
  scale_y_continuous(labels = scales::percent)

## Tree depth check (default tree depth is 10)
mack_diagnostics %>% 
  ggplot(aes(iteration, treedepth__, color = chain)) + 
  geom_line() + 
  geom_hline(aes(yintercept = max_treedepth), color = 'red')

## Step size
mack_diagnostics %>% 
  ggplot(aes(iteration, stepsize__, color = chain)) + 
  geom_line() 

## Summary
stan1_summary <- summary(stan1)$summary %>% 
  as.data.frame() %>% 
  mutate(variable = rownames(.)) %>% 
  select(variable, everything()) %>% 
  as_data_frame()

## neff
stan1_summary %>% 
  ggplot(aes(n_eff)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept = 4000), color = 'red')

## rhat (should be < 1.1, Gelman)
stan1_summary %>% 
  ggplot(aes(Rhat)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept = 1.1), color = 'red')

##### BAYESPLOT: PLOTTING MCMC DRAWS #####

## Diagnostics and inference using Bayesplot
## http://mc-stan.org/bayesplot/articles/index.html

## Extract posterior draws
posterior <- as.array(stan1)
dim(posterior)
dimnames(posterior)

## posterior is an array extracted from the stan object
dim(posterior)
posterior[1,1,] # one iteration
y_new_vector <- posterior[1,1,5:66] # one iteration
length(y_new_vector)
length(stat_dat$closure_effect01)
sampled_iterations <- sample(seq(1:1000), 9, replace = FALSE)
y_new_matrix <- posterior[sampled_iterations, 1, 5:66] 

## http://mc-stan.org/bayesplot/articles/plotting-mcmc-draws.html
color_scheme_set("red")

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

rhats <- rhat(stan1, pars = c("alpha","b1", "b2", "b3"))
print(rhats)

## Effective sample size
## The effective sample size is an estimate of the number of independent draws from the posterior distribution of the estimand of interest. Because the draws within a Markov chain are not independent if there is autocorrelation, the effective sample size, neff, will be smaller than the total sample size, N. The larger the ratio of neff to N the better.

ratios_cp <- neff_ratio(stan1, pars = c("alpha","b1", "b2", "b3"))
print(ratios_cp)

##### POSTERIOR PREDICTIVE CHECKS #####

stat_dat %>% count(Nation, closure_effect01)
nation_y <- stat_dat %>% count(Nation, closure_effect01)

# Get vector of y outcome values
y <- stat_dat$closure_effect01
nation <- stat_dat$Nation01

## Extract mcmc iterations in long format
stan1_mcmc <- stan1 %>% rstan::extract()
summary(stan1_mcmc)
head(stan1_mcmc$alpha)
str(stan1_mcmc$b1)
head(stan1_mcmc$y_new)

stan1_pars <- stan1_mcmc[ c("alpha", "b1", "b2", "b3")] %>% 
  map_df(as_data_frame, .id = 'variable')
stan1_pars

stan1_pars %>% 
  ggplot(aes(value, fill = variable)) + 
  geom_density() + 
  facet_wrap(~variable, scales = 'free') + 
  coord_flip()

pp_ynew_matrix <- stan1_mcmc$y_new

## Create a dataframe
pp_ynew <- stan1_mcmc['y_new'] %>% 
  map_df(as_data_frame, .id = 'variable') %>% 
  gather(observation, value, -variable) %>% 
  mutate(iteration = rep(1:4000, 62))

## Get observation ID to match with empirical data
pp_ynew <- pp_ynew %>%
  mutate(obs_id = as.numeric(substring(observation, 2)))
       
## Join with empirical data on Nations
pp_ynew <- stat_dat %>% select(obs_id, closure_effect01, Nation) %>% 
  left_join(pp_ynew, ., by = "obs_id")

##### PLOT BY OBS ID

stat_dat %>% 
  ggplot(aes(obs_id, closure_effect01, color = Nation)) + 
  geom_point()

pp_ynew_obsid_counts <- pp_ynew %>% 
  count(obs_id, value) 

pp_ynew_obsid_counts_wide <- pp_ynew_obsid_counts %>% 
  spread(key = value, value = n) %>% 
  mutate(prop_affected = `1` / (`0` + `1`))

pp_ynew_obsid_counts

pp_ynew_obsid_counts %>% 
  ggplot(aes(obs_id, value, size = n)) + 
  geom_point(alpha = 0.2) + 
  geom_point(data = stat_dat, aes(obs_id, closure_effect01, color = Nation, size = NULL)) + 
  geom_point(data = pp_ynew_obsid_counts_wide, 
             aes(obs_id, prop_affected, color = NULL, size = NULL))

pp_ynew %>% 
  ggplot(aes(obs_id, value, color = Nation)) + 
  geom_point(alpha = 0.01)

##### PLOT BY NATION

## Get those affected by nation and iteration
pp_ynew_nation_counts <- pp_ynew %>% 
  count(iteration, Nation, value) 

## Complete the dataset and select affected (value = 1)
pp_ynew_nation_counts2 <- pp_ynew_nation_counts %>% 
  complete(iteration, Nation, value, fill = list(n = 0)) %>% 
  filter(value == 1)

nation_y_affected <- nation_y %>% filter(closure_effect01 == 1)

pp_ynew_nation_counts2 %>% 
  ggplot(aes(n, fill = Nation)) + 
  geom_histogram(binwidth = 1, color = "black", alpha = 0.5) + 
  geom_vline(data = nation_y_affected, aes(xintercept = n, color = Nation), size = 2) + 
  labs(title = "Were you affected by the closure?")

