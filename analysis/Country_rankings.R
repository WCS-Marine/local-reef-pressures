library(tidyverse)
library(here)
library(janitor)

data <- read_csv(here("extreme_per_country.csv")) %>% 
  clean_names()

data

??dplyr::slice

#countries with most pressures
data %>% 
  arrange(pressure,
          -extreme_count) %>%
  group_by(pressure) %>% 
  slice_max(extreme_count, n=10) %>% 
  select(pressure, 
         eez, 
         starts_with("extreme")) 

 .Last.value %>% 
  write_csv(here("extreme_top10_countries.csv"))

#countries with proportion of most pressures
data %>% 
  arrange(pressure,
          -extreme_fraction) %>%
  group_by(pressure) %>% 
  slice_max(extreme_fraction, n=10) %>% 
  select(pressure, 
         eez, 
         starts_with("extreme")) 

             