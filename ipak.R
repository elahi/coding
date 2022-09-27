################################################################################
##' @title Package installation
##' @author Robin Elahi
##' @date 2017-05-01
##' @log 2022-09-27 updated packages
################################################################################

# https://gist.github.com/stevenworthington/3178163

# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("tidyverse", "lme4", "nlme")
ipak(packages)

# rstan
# first install C++ toolchain
# https://github.com/rmacoslib/r-macos-rtools#how-do-i-use-the-installer 
# note that the above probably works even though there is a failure message at the end; see
# https://discourse.mc-stan.org/t/friendly-mac-c-installer-does-not-work-for-me-whats-another-option/27272/4
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# verify installation
example(stan_model, package = "rstan", run.dontrun = TRUE)
summary(fit)

# cmdstan
# https://mc-stan.org/cmdstanr/articles/cmdstanr.html
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(cmdstanr)
install_cmdstan()
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")
check_cmdstan_toolchain()
cmdstan_path()
cmdstan_version()
file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
file
mod <- cmdstan_model(file)
mod$print
mod$exe_file()
# names correspond to the data block in the Stan program
data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))

fit <- mod$sample(
  data = data_list, 
  seed = 123, 
  chains = 4, 
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

fit$summary()

# rethinking
install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
devtools::install_github("rmcelreath/rethinking")

# bayes rules
install.packages(c("bayesrules", "janitor", "rstanarm",
                   "bayesplot", "tidybayes", "broom.mixed", "modelr",
                   "e1071", "forcats"), 
                 dependencies = TRUE)
