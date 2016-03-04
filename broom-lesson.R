##' Robin Elahi
##' 4 March 2016
##' Joey Bernhardt's Broom lesson
##' https://github.com/JoeyBernhardt/oconnorlab-stats/blob/master/Rscripts/broom-lesson.Rmd
##' 

library(gapminder)
library(broom)
library(dplyr)
library(ggplot2)

##### Exploring broom basics #####

head(gapminder)

fm <- lm(lifeExp ~ gdpPercap, data = gapminder)

str(fm)

summary(fm)

coef(fm)

tidy(fm)

glance(fm)

augment(fm) %>% head()

augment(fm) %>% class

augment(fm) %>% filter(lifeExp < 30)

##### Fitting a model over different groups #####

mod1 <- gapminder %>% group_by(country) %>%
  do(tidy(lm(lifeExp ~ gdpPercap, data = .)))

mod1 %>% filter(term != "(Intercept)" & p.value < 0.05) %>%
  ungroup %>% arrange(-estimate)

# Add confidence intervals

gapminder %>% 
  group_by(country) %>% 
  do(tidy(lm(lifeExp ~ gdpPercap, data = .), conf.int = TRUE))


##### Visualizing model output #####

my_results_to_viz <- 
  gapminder %>% 
  group_by(year) %>%
  do(tidy(lm(lifeExp ~ gdpPercap, data = .), conf.int =T))

my_results_to_viz %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(y = estimate, x = factor(year))) + geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high))
