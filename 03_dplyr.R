
##### 03-dplyr #####

library(tidyverse)

surveys <- read_csv("data/portal_data_joined.csv")

surveys %>% 
  filter(weight < 5) %>% 
  select(species_id, sex, weight)

surveys

surveys_gw <- surveys %>%
  filter(!is.na(weight)) %>%
  group_by(genus, plot_id) %>%
  summarize(mean_weight = mean(weight))

surveys_gw
str(surveys_gw)

surveys_spread <- surveys_gw %>%
  spread(key = genus, value = mean_weight)

surveys_spread
str(surveys_spread)

surveys_gather <- surveys_spread %>%
  gather(key = genus, value = mean_weight, -plot_id)

str(surveys_gather)

surveys_spread %>%
  gather(key = genus, value = mean_weight)

surveys_complete <- surveys %>%
  filter(species_id != "",         # remove missing species_id
         !is.na(weight),           # remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         sex != "")                # remove missing sex


species_counts <- surveys_complete %>% 
  group_by(species_id) %>% 
  tally() %>% 
  filter(n >= 50)

surveys_complete <- surveys_complete %>% 
  filter(species_id %in% species_counts$species_id)

write_csv(surveys_complete, path = "carpentry_training/data_output/surveys_complete.csv")




