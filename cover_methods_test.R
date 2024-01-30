################################################################################
##' @title Comparing cover data by observer
##' @author Robin Elahi
##' @date 2024-01-29
##' @log 
################################################################################

#### Packages ####
library(tidyverse)
library(readxl)

# Data
d <- read_xlsx("data/methods_test_compiled.xlsx", sheet = "cover")

d_long <- d %>% 
  gather(key = "taxon", value = "cover", antho:tar) %>% 
  mutate(quad_year = paste(quadrat, year, sep = "_"), 
         quadrat = as.character(quadrat),
         year = as.character(year))

# Observed taxa
d_mean <- d_long %>% 
  group_by(taxon) %>%
  summarise(mean = mean(cover)) %>% 
  filter(mean > 0)

d_long <- left_join(d_long, d_mean, by = "taxon")

d_long %>% 
  filter(mean > 0) %>% 
  ggplot(aes(observer, cover, color = year, shape = quadrat, group = quad_year)) + 
  geom_point(size = 4, alpha = 0.7) +
  geom_line() + 
  facet_wrap(~ taxon) +
  theme_bw()

ggsave("figs/cover_methods_test.pdf", height = 8.5, width = 11)
