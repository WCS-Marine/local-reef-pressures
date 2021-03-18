# Sensitivity analysis to buffer size

rm(list=ls())

library(raster)
# library(rgdal)
library(rgeos)
library(mSpatial) # https://github.com/townleym/mSpatial/
library(here)
library(spatial.tools)
source(here::here("scripts", "gPolyByIntersect2.R"))
source(here::here("scripts", "IntersectByTile.R"))
library(sf)
library(tidyverse)
library(corrgram)


# Using Mercator CRS centered on meridian 150E
EPSG <- make_EPSG()
id <- which(EPSG$code == 3832)
new_proj <- EPSG$prj4[id]

load(here::here("data","allreefs.RData"))
a <- select(allreefs, OBJECTID, pop_count, pop_count_raw)
a <- as_Spatial(a)
# Buffering
a_buffer_1 <- gBuffer(a, byid = T, width = 1000)
a_buffer_5 <- gBuffer(a, byid = T, width = 5000)
a_buffer_10 <- gBuffer(a, byid = T, width = 10000)
a_buffer_15 <- gBuffer(a, byid = T, width = 15000)
a_buffer_20 <- gBuffer(a, byid = T, width = 20000)
a_buffer_50 <- gBuffer(a, byid = T, width = 50000)
a_buffer_100 <- gBuffer(a, byid = T, width = 100000)


### 3. COASTAL DEVELOPMENT: POPULATION COUNT
# Read population count
b <- raster(paste0(getwd(),"/../reef-context/data-raw/coastal-development/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_2pt5_min.tif"))
p <- rasterToPoints(b, spatial = T)
names(p) <- "values"
p <- spTransform(p, CRS(new_proj))
rm(b)
# Calculate pop count by tile (doing it on the entire SpatialPolygons might crash)
a$pop_count_1 <- IntersectByTile(a_buffer_1, p, step=1e6)
a$pop_count_5 <- IntersectByTile(a_buffer_5, p, step=1e6)
a$pop_count_10 <- IntersectByTile(a_buffer_10, p, step=1e6)
a$pop_count_15 <- IntersectByTile(a_buffer_15, p, step=1e6)
a$pop_count_20 <- IntersectByTile(a_buffer_20, p, step=1e6)
a$pop_count_50 <- IntersectByTile(a_buffer_50, p, step=1e6)
a$pop_count_100 <- IntersectByTile(a_buffer_100, p, step=1e6)

pop_count_raw <- a@data[,4:10]
pop_count <- pop_count_raw
calc.percentiles <- function(x) {
  y <- ecdf(x)(x)
  id.0 <- which(x == 0)
  y[id.0] <- 0
  y
}
for (i in 1 : 7) {
  pop_count[,i] <- calc.percentiles(pop_count_raw[,i])
}
names(pop_count) <- c("1 km","5 km","10 km","15 km","20 km","50 km","100 km")
corrgram(pop_count, lower.panel=NULL,
         upper.panel=panel.cor,
         cor.method="spearman")
# Saved as Figure S12
