library(sf)

# Transforming the Mercator Pacific Centred into the "standard" WGS84 creatres problems with some polygons

# The problem comes from polygons touching the 180 degree line (the antimeridian)
# In Pacific-centred Mercator, the x coordinate of the 180 degree meridian is 3339585
# The solution is to shift the polygon westward by a small amount, so that it does not touch the antimeridian anymore

# These are the polygons creating the problem
# I identified them graphically in QGis, but there should be a way to identify them
# automatically in R by extracting the coordinates or the bounding boxes
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
rgdal::writeOGR(as_Spatial(a_WGS84), here::here("data"), "allreefs_WGS84", "ESRI Shapefile")
sf::st_write(a_WGS84, dsn = paste0(getwd(), "/data/allreefs_WGS84.gpkg"), driver = "GPKG")
allreefs <- a_WGS84
save(allreefs, file = here::here("data", "allreefs_WGS84.RData"))
