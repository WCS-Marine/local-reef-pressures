
rm(list=ls())

library(raster)
library(rgdal)
library(rgeos)
library(mSpatial) # https://github.com/townleym/mSpatial/
library(here)
library(spatial.tools)
source(here::here("scripts","gPolyByIntersect2.R"))
source(here::here("scripts","IntersectByTile.R"))
library(sf)

# Using Mercator CRS centered on meridian 150E
EPSG <- make_EPSG()
id <- which(EPSG$code == 3832)
new_proj <- EPSG$prj4[id]

# Load coral reef layer from Beyer et al 2018
allreefs <- readOGR(here::here("data-raw", "50-reefs"),"allreefs")

# If needed, reproject allreefs into the new projection
# allreefs <- spTransform(allreefs, CRS(new_proj))

# Create a buffer of 5000 m around each reef cell
allreefs_buffer <- gBuffer(allreefs, byid = T, width = 5000)
#allreefs_buffer <- readOGR(here::here("data-raw", "50-reefs"),"allreefs_buffer")

#(I think saving it in ESRI Shapefile changes the proj4string a bit - hence the warning in IntersectByTile -but the substantial CRS is the same)
# Reconverting to the new_proj does not solve the problem
# allreefs <- spTransform(allreefs, CRS(new_proj))
# allreefs_buffer <- spTransform(allreefs_buffer, CRS(new_proj))

#### I STOP HERE, NEED TO UPLOAD FILES IN THE REPOSITORY - APRIL 20TH 2021


### 1. FISHING: GRAVITY
grav <- readOGR(here::here("data-raw", "fishing", "cinner-gravity"), "Global Gravity of Coral Reefs 2.0")
# Grav_NP and Grav_NC are the fields
grav <- spTransform(grav, CRS(new_proj))
is <- gIntersects(allreefs, grav, byid = T)
table(colSums(is)) # 1205 out of 54596 (2%) reefs not intersecting any gravity cells. They will be NA
grav_NP <- grav_NC <- rep(NA, dim(is)[2])
for (i.reef in 1:dim(is)[2]) {
  if (i.reef %% 500 == 0 ) cat(i.reef,"\n"); flush.console()
  id <- which(is[, i.reef])
  if (length(id) > 0) {
    grav_NP[i.reef] <- mean(grav$Grav_NP[id])
    grav_NC[i.reef] <- mean(grav$Grav_NC[id])
  }
}
allreefs$grav_NP <- grav_NP
allreefs$grav_NC <- grav_NC
writeOGR(allreefs, here::here("data-raw", "50-reefs"),"allreefs", driver="ESRI Shapefile", overwrite = TRUE)


### 2. COASTAL DEVELOPMENT: POPULATION COUNT
# Read population count
a <- raster(here::here("data-raw", "coastal-development", "gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_2pt5_min.tif"))
p <- rasterToPoints(a, spatial = T)
names(p) <- "values"
p <- spTransform(p, CRS(new_proj))
rm(a)
# Calculate pop count by tile (doing it on the entire SpatialPolygons might crash)
pop_count <- IntersectByTile(allreefs_buffer, p, step=1e6)
allreefs$pop_count <- pop_count
writeOGR(allreefs, here::here("data-raw", "50-reefs"),"allreefs", driver="ESRI Shapefile", overwrite = TRUE)

# tile[1,1] <- -12e6
# tile[1,2] <- -11e6
# tile[2,1] <- -2e6
# tile[2,2] <- -1e6


### 3. INDUSTRIAL PRESSURE: NUMBER OF PORTS
ports <- readOGR(here::here("data-raw", "industrial-pressure", "ports.shp"))
ports <- spTransform(ports, CRS(new_proj))
# Intersect the ports with the buffers
is <- gIntersects(allreefs_buffer, ports, byid = T)
allreefs$num_ports <- colSums(is)
writeOGR(allreefs, here::here("data-raw", "50-reefs"),"allreefs", driver="ESRI Shapefile", overwrite = TRUE)



### 4. TOURISM
# Read reef value
a <- raster(here::here("data-raw", "tourism", "reef_value1.tif"))
p <- rasterToPoints(a, spatial = T)
names(p) <- "values"
p <- spTransform(p, CRS(new_proj))
rm(a)
# Calculate pop count by tile (doing it on the entire SpatialPolygons might crash)
reef_value <- IntersectByTile(allreefs, p, step=1e6)
allreefs$reef_value <- reef_value
writeOGR(allreefs, here::here("data-raw", "50-reefs"),"allreefs", driver="ESRI Shapefile", overwrite = TRUE)



### 5. WATER POLLUTION: SEDIMENTS
a <- raster(here("data-raw","water-quality","sed_plume_avg","sed_plume_avg.tif"))
# Get centroids and reproject them in the same CRS of the raster
allreefs_centroids <- gCentroid(allreefs, byid=T)
allreefs_centroids_Mol <- spTransform(allreefs_centroids, CRS(proj4string(a)))
 # --- for sf
allreefs_centroids <- st_centroid(allreefs, byid=T)
allreefs_centroids_Mol <- st_transform(allreefs_centroids, CRS(proj4string(a)))
 # ---
# Extract values
sed <- extract(a,allreefs_centroids_Mol)
length(which(is.na(sed)))
length(which(sed==0)) / length(allreefs)
# 220 reefs with NA; 42% are zero sediment
allreefs$sediment <- sed
writeOGR(allreefs, here::here("data-raw", "50-reefs"),"allreefs", driver="ESRI Shapefile", overwrite = TRUE)



### 6. WATER POLLUTION: NITROGEN
a <- raster(here("data-raw","water-quality","nut_plume_avg","nut_plume_avg.tif"))
a.ton <- a / 1000 # Converting kg to tons
# Get centroids and reproject them in the same CRS of the raster
allreefs_centroids <- gCentroid(allreefs, byid=T)
allreefs_centroids_Mol <- spTransform(allreefs_centroids, CRS(proj4string(a.ton)))
 # -- for sf
allreefs_centroids <- st_centroid(allreefs, byid=T)
allreefs_centroids_Mol <- st_transform(allreefs_centroids, CRS(proj4string(a.ton)))
 #
# Extract values
nut <- extract(a.ton, allreefs_centroids_Mol)
hist(nut,plot=F) # Some very high values!
allreefs$nutrient <- nut
writeOGR(allreefs, here::here("data"), "allreefs", "ESRI Shapefile", overwrite = TRUE)










### SAVE FINAL DATA
writeOGR(allreefs, here::here("data"), "allreefs", "ESRI Shapefile")
 # -- for sf
 st_write(allreefs, here::here("data"), "allreefs", driver = "ESRI Shapefile")


# # Compare with old values
# old <- read_sf(here::here("data","old"),"allreefs")
# difnut <- allreefs$nutrient - old$nutrient
# hist(difnut)
# summary(difnut[difnut!=0])
# difsed <- allreefs$sediment - old$sediment
# hist(difsed)
# summary(difsed[difsed!=0])

