# - calculate statistics per EEZ

rm(list=ls())

library(sf)
library(here)
library(tidyverse)

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
a <- st_join(allreefs,eez1,largest=T)

save(a,file="SpatialJoinEEZ_allreefs.RData")

eez_tropical <- st_join(eez1,
                        st_as_sf(
                          st_as_sfc(
                            st_bbox(allreefs)
                          )
                        )
)