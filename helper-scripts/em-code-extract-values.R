#em code on extract values

library(here)
library(tidyverse)
library(readr)

#load extract values script
source(here("helper-scripts", "extract_values.R"))

#' # Open allreefs data
load(here("data", "allreefs.RData"))
allreefs <- sf::as_Spatial(allreefs)

#' @examples
#' # Read data
data <- read_csv(here("helper-scripts","fishbelt-export.csv")) %>% 
  select(country, 
         site, 
         longitude, 
         latitude)
data


#' # Extract the values of the indicators
indicators <- extract_values(data, allreefs, max.radius = 20000) %>% 
  as_tibble()


library(skimr)
skim(indicators$score)
skim(indicators$cumul_score)

?facet_grid

skim(indicators)

#climate score - high is 50 Reefs, low is not 50 Reefs

indicators %>% 
  mutate(climate_score = scorecn + scorepfc + scoreth + scoretr) %>% 
  ggplot(aes(x = climate_score, y = cumul_score)) + 
  geom_point(aes(colour = country)) + 
  theme_minimal() + 
  geom_vline(xintercept = 0.0379, lty = 2, size = 0.25) + 
  geom_hline(yintercept = 0.463, lty = 2, size = 0.25) + 
  facet_grid(.~country, scales = "free", space = "fixed") + 
  labs(x = "50 Reefs climate score (high-refugia)",  
       y = "Local threat score") + 
  theme(legend.position = "none")


  
