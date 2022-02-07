# Post treatment

rm(list = ls())

library(sf)
library(here)

load(here("data", "allreefs.RData"))

# Names of the six pressures
threat_names <- c(
  "Fishing", "Coastal pop", "Industrial dev",
  "Tourism", "Sediments", "Nitrogen"
)

top_threat_new <- threat_names[allreefs$top_threat]
allreefs$top_threat <- top_threat_new

# Save final dataset in three formats
save(allreefs, file = here::here("data", "allreefs.RData"))
rgdal::writeOGR(as_Spatial(allreefs), here::here("data"), "allreefs", "ESRI Shapefile", overwrite=T)
sf::st_write(allreefs, dsn = paste0(getwd(), "/data/allreefs.gpkg"), driver = "GPKG", delete_layer = T)

# "problematic" polygons - see /R/Transform_CRS.R for explanation
pol_id <- c(45685,
46184,
46271,
46336,
46718,
46802,
46880,
46945,
47024,
47112,
47195,
47278,
47338,
47474,
49039,
49282,
49329,
49376)

# Here we make a copy of the allreefs dataset
a <- allreefs
# Loop on the problematic polygons
for (pol.id.i in pol_id) {
  # Shift the polygon westward by one unit (in PDC CRS, this is one meter)
  a$geometry[pol.id.i] <- a$geometry[pol.id.i]-c(1,0)
  # Reassign it the Pacific centred CRS because the shifting erases the projection
  a$geometry[pol.id.i] <- st_set_crs(a$geometry[pol.id.i], 3832)
}

# Transform the entire dataset into the WGS84 CRS
a_WGS84 <- st_transform(a,crs=4326)

# Save the new layers in three formats
rgdal::writeOGR(as_Spatial(a_WGS84), here::here("data"), "allreefs_WGS84", "ESRI Shapefile", overwrite = T)
sf::st_write(a_WGS84, dsn = paste0(getwd(), "/data/allreefs_WGS84.gpkg"), driver = "GPKG", delete_layer = T)
allreefs <- a_WGS84
save(allreefs, file = here::here("data", "allreefs_WGS84.RData"))
