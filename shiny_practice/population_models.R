################################################################################
##' @title Basic population models for ignited lessons
##' @author Robin Elahi
##' @contact elahi.robin@stanford.edu
##' @date 2020-07-08
################################################################################

library(tidyverse)

##### LOGISTIC MAP, DISCRETIZATION OF THE LOGISTIC MODEL #####

## This script adapted from:
## http://biom300.weebly.com/logistic-model.html

## We want to define a function that calculates the right hand side of the discrete time logistic model. Such a function will take two 'arguments': the current population size and the parameter values. Let 'n' be the current population size, and 'p' be a parameter vector containing two terms, the growth rate (r) and the carrying capacity (K)

## First specify the initial population size and the parameters
n0 <- 1
p <- c(r = 0.5, K = 100) ## c() is a function that simply concatenates the two entries, creating a vector with two entries
p[1] ## prints the first entry in the parameter vector
p[2] ## prints the second entry in the parameter vector 
## Note that the entries of p are "named" r and K
names(p)
## Naming gives us a second way to refer to each parameter
p['r']
p['K']

## Naming the parameters is really useful, especially when simulating models with lots of parameters, because it keeps you from having to remember which entry in the vector corresponds to each parameter.

## Now we can define our function
disc.logistic <- function(n, p) { ## here are just saying that 'disc.logistic' is a function that takes two arguments
  r <- unname(p['r']) ## set the growth rate (dropping the parameter name with 'unname')
  K <-  unname(p['K']) ## set the carrying capacity
  n1 <- n + r * n * (1 - n / K) ## calculate the new population size
  return(n1) ## and return it 
}

## So, to calculate the population size at time t=1, 
n1 <- disc.logistic(n0, p) ## call the function with the population size at t=0 and the parameters
n1

## To calculate the population size at t=2, call the function with n=n1 and the parameters
n2 <- disc.logistic(n1, p)
n2

## You can continue in this fashion forever, but of course that is pretty annoying because you have to specify the value of 'n' anew each time. A better way to do it would be to create a function that only required you to pass in the initial population size, the parameters, and how many time steps you want to simulate.

disc.logistic.2 <- function(n, p, T) { ## T is the total number of time steps
  
  ## Create a new variable that is a vector of length T, so that N[1] is the initial population size (t=0), N[2] is the population size at time t=1, N[3] = pop'n size at t=2, ..., N[T] = pop'n size at t=T-1 and N[T+1] = pop'n size at t=T
  N <- vector(length=T+1)
  N[1] <- n ## set the initial value
  
  ## Create a loop that will increment time by one time unit, calculate the new population size, and save it in our vector
  for (t in 1:T) { 
    N[t+1] <- disc.logistic(N[t], p) ## we can call the function we wrote above to calculate the new population size
  }
  return(N) ## return the vector of population sizes
}


n <- disc.logistic.2(n0, p, 10) ## calculate the first 10 time steps
n

## With this function, we can simulate the model for any number of timesteps we wish. We might also want to plot the results. This can be done with the function plot(). Type ?plot in the R console to see all of the available options for plotting. The best way to plot is to specify both the x and y values. In this case, the x-axis is time, so the x values are the time steps, and the y-axis is population size. 0:10 generates a sequence of integers from 0 to 10 (our time steps). type='l' tells R to make a line plot (as opposed to a dot plot), and the xlab and ylab options sets the x- and y-axis labels.
plot(x=0:10, y=n, type='l', xlab='Time', ylab='Population size', main='Discrete time logistic model')

## Experimenting with parameters
n0 <- 1
p <- c(r = 0.5, K = 50) 
n_years <- 50
n <- disc.logistic.2(n = n0, p = p, T = n_years)
plot(x = 0:n_years, y = n, type = 'l', 
     xlab = 'Time', ylab = 'Population size', 
     main = 'Discrete time logistic model')

df <- tibble(x = seq(0, n_years), 
             n = n)

df %>% 
  ggplot(aes(x = x, y = n)) + 
  geom_line() + 
  labs(x = "Time", y = "Population size", 
       title = "Discrete time logistic model") +
  scale_y_log10()

## Plot population growth rate against population size
df <- tibble(t = 1:length(n), 
             n = n, 
             n_lead = lead(n), 
             n_change = n_lead - n, 
             per_capita_change = n_change / n, 
             pop_growth_rate = per_capita_change * n)
write.csv(df, "shiny_practice/pop_models.csv")

ggplot(data = df, mapping = aes(x = t, y = n)) +
  geom_point() + 
  geom_line()

df %>% 
  ggplot(data = ., mapping = aes(x = t, y = n)) +
  geom_point() + 
  geom_line()

df %>% 
  ggplot(aes(t, n)) + 
  geom_point() + 
  geom_line()

df %>% 
  ggplot(aes(n, per_capita_change)) + 
  geom_line()

df %>% 
  ggplot(aes(n, pop_growth_rate)) + 
  geom_point() + 
  geom_line()
ggsave(filename = "shiny_practice/figs/pgr-n-plot.png", 
       height = 3.5, width = 3.5)

plot(df$n, df$pop_growth_rate, type = "b")

# 
# ##### CORRECTIONS FROM GIULIO #####
# 
# bh_logistic <- function(n, p) { ## here are just saying that 'disc.logistic' is a function that takes two arguments
#   r <- unname(p['r']) ## set the growth rate (dropping the parameter name with 'unname')
#   K <-  unname(p['K']) ## set the carrying capacity
#   n1 <- (exp(r) * n) / ((1 + ((exp(r) - 1) / K)) * n)  ## calculate the new population size
#   return(n1) ## and return it 
# }
# 
# bh_logistic2 <- function(n, p, T) { ## T is the total number of time steps
#   N <- vector(length = T + 1)
#   N[1] <- n ## set the initial value
#   for (t in 1:T) { 
#     N[t + 1] <- bh_logistic(n = N[t], p = p)
#   }
#   return(N) ## return the vector of population sizes
# }
# 
# ## Experimenting with parameters
# n0 <- 1
# p <- c(r = 0.25, K = 20) 
# n_years <- 50
# 
# bh_logistic(n = n0, p = p)
# 
# n <- bh_logistic2(n = n0, p = p, T = n_years)
# 
# plot(x = 0:n_years, y = n, type = 'l', 
#      xlab = 'Time', ylab = 'Population size', 
#      main = 'Discrete time logistic model')
