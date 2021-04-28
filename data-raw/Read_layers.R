library(raster)
library(rgdal)
library(rgeos)
# devtools::install_github("townleym/mSpatial")
library(mSpatial)
library(here)
# Since it seems to have recently been removed from CRAN:
# devtools::install_github("cran/spatial.tools") 
library(spatial.tools)
library(sf)

source(here::here("R","gPolyByIntersect2.R"))
source(here::here("R","IntersectByTile.R"))

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


# 1. FISHING: MARKET GRAVITY
grav <- readOGR(here::here("data-raw", "fishing"), "Global Gravity of Coral Reefs 2.0")
# Grav_NC is market gravity
grav <- spTransform(grav, CRS(new_proj))
is <- gIntersects(allreefs, grav, byid = T)
table(colSums(is)) # 1205 out of 54596 (2%) reefs not intersecting any gravity cells. They will be NA
grav_NC <- rep(NA, dim(is)[2])
for (i.reef in 1:dim(is)[2]) {
  if (i.reef %% 500 == 0 ) cat(i.reef,"\n"); flush.console()
  id <- which(is[, i.reef])
  if (length(id) > 0) {
    grav_NC[i.reef] <- mean(grav$Grav_NC[id])
  }
}
allreefs$grav_NC <- grav_NC

# 2. COASTAL DEVELOPMENT: POPULATION COUNT
# The original data layer is available from the CIESIN website
# https://doi.org/10.7927/H4PN93PB
# Note that downloading the data requires registration
# Select "Single Year" for Temporal, "GeoTIFF" for FileFormat, "2.5 minutes" for Resolution, then tick "Year 2020"
a <- raster(here::here("data-raw", "coastal-development", "gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_2pt5_min.tif"))
p <- rasterToPoints(a, spatial = T)
names(p) <- "values"
p <- spTransform(p, CRS(new_proj))
rm(a)
# Calculate population count by tile (doing it on the entire SpatialPolygons might crash)
pop_count <- IntersectByTile(allreefs_buffer, p, step=1e6)
allreefs$pop_count <- pop_count


# 3. INDUSTRIAL PRESSURE: NUMBER OF PORTS
# The original data layer is available at
# https://goo.gl/Yu8xxt
# and can be downloaded as kml.
ports <- read_sf(here::here("data-raw", "industrial-pressure", "World Ports.kml"))
ports <- as_Spatial(ports)
ports <- spTransform(ports, CRS(new_proj))
# Intersect the ports with the buffers
is <- gIntersects(allreefs_buffer, ports, byid = T)
allreefs$num_ports <- colSums(is)


### 4. TOURISM: REEF VALUE
# The original data layer was available upon request by emailing oceanwealth@tnc.org (See the following link under FAQs)
# https://oceanwealth.org/resources/atlas-of-ocean-wealth/
a <- raster(here::here("data-raw", "tourism", "reef_value.tif"))
p <- rasterToPoints(a, spatial = T)
names(p) <- "values"
p <- spTransform(p, CRS(new_proj))
rm(a)
# Calculate pop count by tile (doing it on the entire SpatialPolygons might crash)
reef_value <- IntersectByTile(allreefs, p, step=1e6)
allreefs$reef_value <- reef_value


# 5. WATER POLLUTION: SEDIMENTS
# This layer was produced in this work
# (code to produce it from raw data will be uploaded later, when Amelia Wenger comes back from maternity leave)

# First unzip the sed_plume_avg.zip file in local-reef-pressures\data-raw\sediments into the same folder
unzip(here("data-raw", "sediments", "sed_plume_avg.zip"), exdir = here("data-raw", "sediments"))

a <- raster(here("data-raw","sediments","sed_plume_avg.tif"))
# Get centroids and reproject them in the same CRS of the raster
allreefs_centroids <- gCentroid(allreefs, byid=T)
allreefs_centroids_Mol <- spTransform(allreefs_centroids, CRS(proj4string(a)))
# Extract values
sed <- extract(a,allreefs_centroids_Mol)
length(which(is.na(sed)))
length(which(sed==0)) / length(allreefs)
# 220 reefs with NA; 42% are zero sediment
allreefs$sediment <- sed


# 6. WATER POLLUTION: NITROGEN
# This layer was produced in this work
# (code to produce it from raw data will be uploaded later, when Amelia Wenger comes back from maternity leave)

# First unzip the nit_plume_avg.zip file in local-reef-pressures\data-raw\nitrogen into the same folder
unzip(here("data-raw", "nitrogen", "nit_plume_avg.zip"), exdir = here("data-raw", "nitrogen"))

a <- raster(here("data-raw","nitrogen","nit_plume_avg.tif"))
a.ton <- a / 1000 # Converting kg to tons
# Get centroids and reproject them in the same CRS of the raster
allreefs_centroids <- gCentroid(allreefs, byid=T)
allreefs_centroids_Mol <- spTransform(allreefs_centroids, CRS(proj4string(a.ton)))
# Extract values
nut <- extract(a.ton, allreefs_centroids_Mol)
hist(nut,plot=F) # Some very high values!
allreefs$nutrient <- nut

# Save data
writeOGR(allreefs, here::here("data"), "allreefs", "ESRI Shapefile", overwrite = TRUE)
save(allreefs, file=here::here("data","allreefs.RData"))

