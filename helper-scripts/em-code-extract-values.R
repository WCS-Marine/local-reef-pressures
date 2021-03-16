#em code on extract values

library(here)
library(tidyverse)
library(readr)

#load extract values script
source(here("helper-scripts", "extract_values.R"))

allreefs <- rgdal::readOGR(here("data","allreefs"))

#' @examples
#' # Read data
data <- read_csv(here("helper-scripts","fishbelt-export.csv")) %>% 
  select(country, 
         site, 
         longitude, 
         latitude)
data

#' # Open allreefs data
allreefs <- rgdal::readOGR(here::here("data"),"allreefs")
names(allreefs)

#' # Extract the values of the indicators
indicators <- extract_values(data, allreefs, max.radius = 20000)
