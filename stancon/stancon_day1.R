#################################################
# Author: Robin Elahi
# Date: 2018-01-10
# StanCon Day 1
#################################################

setwd("stancon/")

library(rstan)
library(rethinking)

##' What is a generative model?
##' 
##' A non-generative model example: where we have weights associated with samples - there is no process that gives rise to these weights, but we use them in our model anyway...
##' If you want to do Bayesian inference, you typically need a generative model
##' (so that you can simulate data)



