##### 05-r-and-databases #####


install.packages(c("dbplyr", "RSQLite"))

library(dplyr)
library(dbplyr)

#dir.create("data", showWarnings = FALSE)
download.file(url = "https://ndownloader.figshare.com/files/2292171",
              destfile = "data/portal_mammals.sqlite", mode = "wb")

mammals <- DBI::dbConnect(RSQLite::SQLite(), "data/portal_mammals.sqlite")

src_dbi(mammals)

tbl(mammals, sql("SELECT year, species_id, plot_id FROM surveys"))

surveys <- tbl(mammals, "surveys")

surveys %>% select(year, species_id, plot_id)

head(surveys, n = 10)

nrow(tbl)
nrow(surveys)

show_query(head(surveys, n = 10))

## Simple database queries

surveys %>% 
  filter(weight < 5) %>% 
  select(species_id, sex, weight)

data_subset <- surveys %>% 
  filter(weight < 5) %>% 
  select(species_id, sex, weight)

data_subset %>% select(-sex)

data_subset <- surveys %>% 
  filter(weight < 5) %>% 
  select(species_id, sex, weight) %>% 
  collect()

data_subset

show_query(surveys %>% 
             filter(weight < 5) %>% 
             select(species_id, sex, weight))

## Complex queries

plots <- tbl(mammals, "plots")
plots
show_query(plots)

surveys

plots %>% 
  filter(plot_id == 1) %>% 
  inner_join(surveys) %>% 
  collect()

show_query(plots %>% 
             filter(plot_id == 1) %>% 
             inner_join(surveys))

## Challenge (page 7)
species <- tbl(mammals, "species")
species

# Return number of rodents observed in each year
species %>% 
  select(species_id, taxa) %>% 
  inner_join(surveys) %>% 
  filter(taxa == "Rodent") %>% 
  group_by(year, plot_id) %>% 
  tally()

show_query(species %>% 
             select(species_id, taxa) %>% 
             inner_join(surveys) %>% 
             filter(taxa == "Rodent") %>% 
             group_by(year, plot_id) %>% 
             tally())  

genus_counts <- left_join(surveys, plots) %>%
  left_join(species) %>%
  group_by(plot_type, genus) %>%
  tally %>%
  collect()

unique_genera <- left_join(surveys, plots) %>%
  left_join(species) %>%
  group_by(plot_type) %>%
  summarize(
    n_genera = n_distinct(genus)
  ) %>%
  collect()

## Creating a new sqlite database

species <- read_csv("carpentry_training/data/species.csv")
surveys <- read_csv("carpentry_training/data/surveys.csv")
plots <- read_csv("carpentry_training/data/plots.csv")

my_db_file <- "portal-database.sqlite"
my_db <- src_sqlite(my_db_file, create = TRUE)
my_db
copy_to(my_db, surveys)
copy_to(my_db, plots)
my_db
