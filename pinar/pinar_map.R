################################################################################
##' @title Map pinar points
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-10-10
################################################################################

library(tidyverse)
df <- read_csv("output/gpx_191010_pinar_edit.csv")

df %>% 
  ggplot(aes(lon, lat)) + 
  geom_point() + 
  coord_equal() + 
  theme_bw()

ggsave("figs/pinar_gps.pdf", height = 7, width = 7)

