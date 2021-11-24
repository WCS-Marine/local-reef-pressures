# Find some examples of locations with high sed/low nutrient
# and vice versa, low sed/high nutrient
library(tmap)

load(here("data", "allreefs.RData"))

nut<-allreefs$nutrient
sed<-allreefs$sediment
allreefs$sednut <- sed-nut
allreefs$nutsed <- nut-sed


countries <- read_sf(here("data-raw", "natural-earth", "cultural"), "ne_10m_admin_0_countries")
countries_proj <- st_transform(countries, st_crs(allreefs))
allreefs_bbox_extended <- st_bbox(allreefs)
allreefs_bbox_extended[4] <- 4.5e6
countries_eq <- st_crop(countries_proj, allreefs_bbox_extended)
rm(allreefs_bbox_extended, countries, countries_proj)

m <- tm_shape(countries_eq) +
  tm_polygons(
    col = "gray",
    border.col = "darkgray",
    lwd = 0.2
  )


m1<-m+tm_shape(allreefs) +
  tm_fill(
    col = "nutsed",
    palette = "OrRd"
  )
tmap_save(m1, filename = "nutsed1.pdf", width = 18.25, height = 4, units = "cm")

m2<-m+tm_shape(allreefs) +
  tm_fill(
    col = "sednut",
    palette = "OrRd"
  )
tmap_save(m2, filename = "sednut.pdf", width = 18.25, height = 4, units = "cm")

# Then cropped and annotated in power point