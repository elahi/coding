library(rethinking)
library(dplyr)

adf <- data.frame(x=runif(100,0,10)) %>%
  mutate(y=rnorm(100,x*2+3, 5),
         y_sd = runif(100,0,10),
         y_obs = rnorm(100, y, y_sd))


precis(lm(y~x, data=adf))
precis(lm(y_obs~x, data=adf))

#here's a straight linear regression
#in rethinking syntax - nice, no?
lm_mod <- alist(
  
  #likelihood
  y ~ dnorm(yhat, sd_y),
  
  #link function
  yhat <- a + b*x,
  
  #priors
  a ~ dnorm(0,10),
  b ~ dnorm(0,10),
  sd_y ~ dunif(0,20)
  
)

lm_fit <- map2stan(lm_mod, data=adf)

#see the result!
precis(lm_fit)



#Now, a model with known observation error
#in y
lm_obs_error_mod <- alist(
  
  #likelihoods
  y_est ~ dnorm(yhat, true_sd_y),
  
  #link function
  yhat <- a + b*x,

  #observation error
  y_obs ~ dnorm(y_est, y_sd),
    
  #priors
  a ~ dnorm(0,10),
  b ~ dnorm(0,10),
  true_sd_y ~ dunif(0,20)
  
)

lm_obs_error_fit <- map2stan(lm_obs_error_mod, data=adf,
                             start=list(y_est = adf$y_obs), #to give size
                             control=list(adapt_delta=0.95),
                             WAIC=FALSE)
#output
precis(lm_obs_error_fit)

#compare
lapply(list(true_lm = lm(y~x, data=adf),
            true_bayesian = lm_fit,
            no_obs_error_lm = lm(y_obs~x, data=adf),
            obs_error_bayesian = lm_obs_error_fit), 
       function(x) precis(x, prob=0.95))


