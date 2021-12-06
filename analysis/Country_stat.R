# - calculate statistics per EEZ

rm(list=ls())

library(sf)
library(here)
library(tidyverse)

# Loading the WGS84 version of allreefs because the EEZ layer has this CRS
load(here("data", "allreefs_WGS84.RData"))

# Download 	Maritime Boundaries v11: World EEZ v11
# from https://www.marineregions.org/downloads.php
eez <- read_sf(here("data-raw","EEZ","eez_v11.gpkg"))

# Join eez attributes to allreefs dataset
eez1 <- select(eez,c("TERRITORY1","TERRITORY2","TERRITORY3"))

# # Search for invalid EEZ
# allreefs1 <- allreefs[1:100,]
# a <- list()
# for (i in 1 : 281){
#   cat(i,"\n")
#   eez2 <- eez1[i,]
#   a[[i]] <- try(st_join(allreefs1,eez2,largest=T))
# }
# # eez 138 (Canada) and 274 (Russia) produce errors. We'll remove them.
# # No reef pixel are within them so this will not have any impact on the results
# rm(allreefs1,eez2,a)

# Remove two eez that produce errors
eez1 <- eez1[-c(138,274),]

# Spatial join (takes long time to run)
# # To speed up, maybe cut eez to the bbox of allreefs?
# # eez_tropical <- st_join(eez1,
#                         st_as_sf(
#                           st_as_sfc(
#                             st_bbox(allreefs)
#                           )
#                         )
# )
a <- st_join(allreefs,eez1,largest=T)

# Save copy of the layer
save(a,file="SpatialJoinEEZ_allreefs.RData")

# Write layer as GPKG to work with QGIS
st_write(a,"allreegfs_with_eez.gpkg")
# In QGIS, I manually assigned eez to reefs that are NA
 
# Read the updated layer
allreefs <- st_read("allreefs_with_eez_2.gpkg")


# "X country has the largest proportional area of reef at high risk to [pressure]."
# Calculate number of reefs assigned with each top pressure, per eez

# For each country and pressure, count the number of pixels above the 75% percentile
# Specify that top_threat and eez are factors
allreefs$top_threat <- factor(allreefs$top_threat)
allreefs$eez <- factor(allreefs$TERRITORY1)
# Set pressure names as in the dataframe
pressures <- c("grav_NC","pop_count","num_ports","reef_value","sediment","nutrient")

# output will be stored in a list
eez_list_n <- eez_list_f <- list()
n.reef <- vector()

# Drop geometry to simplify output
a1 <- st_drop_geometry(allreefs)

# Loop on eez
for (i.eez in 1 : 142) {
  cat(levels(allreefs$eez)[i.eez],"\n")
  
  # Retain only reefs in this eez
  a2 <- filter(a1, eez == levels(eez)[i.eez])
  
  # Number of reef pixel in this eez
  n.reef[i.eez] <- nrow(a2)
  
  # loop on pressures
  n.high <- vector()
  for (i.pressure in 1 : 6) {
    
    # Count the number of pixels that have high values (above the 75th percentile)
    n.high[i.pressure] <- length(which(a2[,pressures[i.pressure]]>0.75))
    
  }
  
  eez_list_n[[i.eez]] <- n.high
  eez_list_f[[i.eez]] <- n.high / n.reef[i.eez]
  names(eez_list_n)[i.eez] <- names(eez_list_f)[i.eez] <- levels(allreefs$eez)[i.eez]

}

# Create a dataframe with eez, total number of reefs, pressure, and number and fraction of extreme reefs per pressure
pressure_clean_names <- c("Fishing","Coastal population","Industrial development","Tourism","Sediments","Nutrients")
extreme_per_country <- 
  data.frame(
    eez = rep(levels(allreefs$eez),each=6),
    n.reef = rep(n.reef,each=6),
    pressure = rep(pressure_clean_names,142),
    extreme_count = unlist(eez_list_n),
    extreme_fraction = unlist(eez_list_f)
  )
row.names(extreme_per_country) <- NULL

# For each pressure, show the eez with the highest proportional values
extreme_per_country %>% filter(pressure == "Fishing") %>% arrange(extreme_fraction)
extreme_per_country %>% filter(pressure == "Coastal population") %>% arrange(extreme_fraction)
extreme_per_country %>% filter(pressure == "Industrial development") %>% arrange(extreme_fraction)
extreme_per_country %>% filter(pressure == "Tourism") %>% arrange(extreme_fraction)
extreme_per_country %>% filter(pressure == "Sediments") %>% arrange(extreme_fraction)
extreme_per_country %>% filter(pressure == "Nutrients") %>% arrange(extreme_fraction)

# Save as csv
write.csv(extreme_per_country,file="extreme_per_country.csv")

