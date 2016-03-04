##' Robin Elahi
##' 4 March 2016
##' tidyr
##' http://blog.rstudio.org/2016/02/02/tidyr-0-4-0/
##' 

library(gapminder)
library(dplyr)
library(ggplot2)
library(tidyr)

gapminder
ggplot(gapminder, aes(year, lifeExp)) + geom_line(aes(group = country))

by_country <- gapminder %>% 
  group_by(continent, country) %>% 
  nest()

by_country

by_country$data[[1]]

# Add a linear model
by_country <- by_country %>% 
  mutate(model = purrr::map(data, ~ lm(lifeExp ~ year, data = .))
  )
by_country

# Unnest
by_country %>% unnest(data)

# Extract model summaries
by_country %>% unnest(model %>% purrr::map(broom::glance))

# Extract coefficients:
by_country %>% unnest(model %>% purrr::map(broom::tidy))

# Extract residuals etc:
by_country %>% unnest(model %>% purrr::map(broom::augment))

# Expanding using complete()
resources <- frame_data(
  ~year, ~metric, ~value,
  1999, "coal", 100,
  2001, "coal", 50,
  2001, "steel", 200
)
resources

resources %>% complete(year, metric)

resources %>% complete(year = full_seq(year, 1L), metric)

# Don't generate the full set of combinations - use nesting()
experiment <- data_frame(
  person = rep(c("Alex", "Robert", "Sam"), c(3, 2, 1)),
  trt  = rep(c("a", "b", "a"), c(3, 2, 1)),
  rep = c(1, 2, 3, 1, 2, 1),
  measurment_1 = runif(6),
  measurment_2 = runif(6)
)
experiment

# Relevant set
experiment %>% complete(nesting(person, trt), rep)

# Full set
experiment %>% complete(person, trt, rep)


