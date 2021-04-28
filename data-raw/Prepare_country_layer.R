# Prepare country layer

rm(list=ls())

library(sf)
library(tidyverse)
library(here)
# library(matrixStats)

load(here::here("data","allreefs.RData")) # just to get the CRS
     
# Read shapefile
countries <- read_sf(here::here("data-raw","natural-earth","cultural"),"ne_10m_admin_0_countries")

# Change CRS for countries to match allreefs
countries_proj <- st_transform(countries, st_crs(allreefs))

# Crop countries to bbox of allreefs
allreefs_bbox_extended <- st_bbox(allreefs)
allreefs_bbox_extended[4] <- 4.5e6
countries_eq <- st_crop(countries_proj, allreefs_bbox_extended)

# Save country later
save(countries_eq , file=here::here("data","countries_eq.RData"))
# save(allreefs_withBCU_prc,
#      allreefs_withBCU_prc_centroids,
#      countries_eq,
#      threats,
#      vthreats,
#      file="DataForAnalysis.RData")

# # Write final shapefiles
# load(here("scripts","DataForAnalysis.RData"))
# names(allreefs_withBCU_prc)
# names(allreefs_withBCU_prc)[15] <- "TERRITORY"
# names(allreefs_withBCU_prc)[22] <- "top_threat"
# names(allreefs_withBCU_prc)[23] <- "cumul_score"
# allreefs_withBCU_prc$MRGID_TER1 <- NULL
# allreefs_withBCU_prc$MRGID_IHO <- NULL
# allreefs_withBCU_prc %>% relocate(c(cumul_score, top_threat), .after = nutrient) -> allreefs_withBCU_prc
# allreefs_withBCU_prc %>% relocate(is.bcu, .before = ReefName) -> allreefs_withBCU_prc
# names(allreefs_withBCU_prc)[12] <- "BCU_name"
# allreefs_withBCU_prc %>% relocate(c(Region, IHO_SEA, TERRITORY), .after = top_threat) -> allreefs_withBCU_prc
# allreefs_withBCU_prc %>% relocate(geometry, .after = nutrient_raw) -> allreefs_withBCU_prc
# st_write(allreefs_withBCU_prc, dsn = paste0(getwd(),"/data/allreefs.gpkg"), driver="GPKG")
# save(allreefs_withBCU_prc, file=paste0(getwd(),"/data/allreefs.RData"))
