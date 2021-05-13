# Read layers
# Reads the original data layers, performs the spatial processing and
# saves the results in the `data/allreefs` layer.
# You do not need to run this script if you are only interested in
# the final results or if you want to extract values for new sites.

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
library(matrixStats)
library(tidyverse)
library(corrgram)

source(here::here("R", "gPolyByIntersect2.R"))
source(here::here("R", "IntersectByTile.R"))

# Using Mercator CRS centered on meridian 150E
EPSG <- rgdal::make_EPSG()
id <- which(EPSG$code == 3832)
new_proj <- EPSG$prj4[id]

# Load coral reef layer from Beyer et al 2018
allreefs <- rgdal::readOGR(here::here("data-raw", "50-reefs"), "allreefs")

# If needed, reproject allreefs into the new projection
# allreefs <- spTransform(allreefs, sp::CRS(new_proj))

# Create a buffer of 5000 m around each reef cell
allreefs_buffer <- rgeos::gBuffer(allreefs, byid = T, width = 5000)
# allreefs_buffer <- rgdal::readOGR(here::here("data-raw", "50-reefs"),"allreefs_buffer")

# (I think saving it in ESRI Shapefile changes the proj4string a bit - hence the warning in IntersectByTile -but the substantial CRS is the same)
# Reconverting to the new_proj does not solve the problem
# allreefs <- spTransform(allreefs, sp::CRS(new_proj))
# allreefs_buffer <- spTransform(allreefs_buffer, sp::CRS(new_proj))



# 1. FISHING: MARKET GRAVITY
grav <- rgdal::readOGR(here::here("data-raw", "fishing"), "Global Gravity of Coral Reefs 2.0") # Field Grav_NC is market gravity

# Transform the market gravity layer into the project CRS
grav <- spTransform(grav, CRS(new_proj))

# Intersect the allreefs layer with the gravity layer
# Do it with the st_intersects function of the sf package, which is more efficient at memory usage
is <- sf::st_intersects(sf::st_as_sf(allreefs), sf::st_as_sf(grav))

# Set a vector containing the mean values
grav_NC <- rep(NA, nrow(allreefs))

# Loop on the elements of the intersection list (is), which correspond to the reef cells
for (i.reef in 1:length(is)) {
  if (i.reef %% 500 == 0) cat(i.reef, "\n")
  flush.console()
  # For each reef cell, take the mean value of all the market gravity cells that intersect with it (is[[i.reef]])
  # and store it in vector grav
  grav_NC[i.reef] <- mean(grav$Grav_NC[is[[i.reef]]])
}

# Replace the NaN with NA
grav_NC[which(is.nan(grav_NC))] <- NA

# Add the grav vector to the allreefs layer
allreefs$grav_NC <- grav_NC
rm(grav, grav_NC, is, i.reef)



# 2. COASTAL DEVELOPMENT: POPULATION COUNT
# The original data layer is available from the CIESIN website
# https://doi.org/10.7927/H4PN93PB
# Note that downloading the data requires registration
# Select "Single Year" for Temporal, "GeoTIFF" for FileFormat, "2.5 minutes" for Resolution, then tick "Year 2020"
a <- raster::raster(here::here("data-raw", "coastal-development", "gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_2pt5_min.tif"))

# Convert raster object to point shapefile
p <- raster::rasterToPoints(a, spatial = T)
names(p) <- "values"

# Transform the population count layer into the project CRS
p <- spTransform(p, sp::CRS(new_proj))
rm(a)

# Calculate population count by tile (doing it on the entire SpatialPolygons might crash)
# And store it in the pop_count vector
# Use the custom function IntersectByTile.R
pop_count <- IntersectByTile(allreefs_buffer, p, step = 1e6)

# Add the pop_count vector to the allreefs layer
allreefs$pop_count <- pop_count
rm(p, pop_count)


# 3. INDUSTRIAL PRESSURE: NUMBER OF PORTS
# The original data layer is available at
# https://goo.gl/Yu8xxt
# and can be downloaded as kml.
ports <- sf::read_sf(here::here("data-raw", "industrial-pressure", "World Ports.kml"))

# Convert the ports layer into a SpatialPolygonsDataFrame object
ports <- sf::as_Spatial(ports)

# Transform the ports layer into the project CRS
ports <- spTransform(ports, sp::CRS(new_proj))

# Intersect the ports layer with the allreef_buffers layers
# We are using the allreefs_buffer layer instead of the allreefs layer
# because we take number of ports within a distance of 5 km from each reef cell
is <- rgeos::gIntersects(allreefs_buffer, ports, byid = T)

# Add the number of ports (calculated as the sum of the intersections) to the allreefs layer
allreefs$num_ports <- colSums(is)
rm(ports, is)



# 4. TOURISM: REEF VALUE
# The original data layer was available upon request by emailing oceanwealth@tnc.org (See the following link under FAQs)
# https://oceanwealth.org/resources/atlas-of-ocean-wealth/
a <- raster::raster(here::here("data-raw", "tourism", "reef_value.tif"))

# Convert raster object to point shapefile
p <- raster::rasterToPoints(a, spatial = T)
names(p) <- "values"

# Transform the tourism layer into the project CRS
p <- spTransform(p, sp::CRS(new_proj))
rm(a)

# Calculate reef value by tile (doing it on the entire SpatialPolygons might crash)
# And store it in the reef_value vector
# Use the custom function IntersectByTile.R
reef_value <- IntersectByTile(allreefs, p, step = 1e6)
allreefs$reef_value <- reef_value
rm(p, reef_value)



# 5. WATER POLLUTION: SEDIMENTS
# This layer was produced in this work
# (code to produce it from raw data will be uploaded later, when Amelia Wenger comes back from maternity leave)

# First unzip the sed_plume_avg.zip file in local-reef-pressures\data-raw\sediments into the same folder
unzip(here("data-raw", "sediments", "sed_plume_avg.zip"), exdir = here("data-raw", "sediments"))
a <- raster::raster(here("data-raw", "sediments", "sed_plume_avg.tif"))

# Calculate the centroids of the reef cells and reproject them in the same CRS of the raster (Mollweide)
allreefs_centroids <- rgeos::gCentroid(allreefs, byid = T)
allreefs_centroids_Mol <- spTransform(allreefs_centroids, sp::CRS(proj4string(a)))

# Read (extract) the values of the sediment layer corresponding to the centroids of the reef cells
# And store it into the sed vector
sed <- raster::extract(a, allreefs_centroids_Mol)

# Look at the values
length(which(is.na(sed)))
length(which(sed == 0)) / length(allreefs)
# 220 reefs with NA; 39.7% are zero sediment

# Add the sed vector to the allreefs layer
allreefs$sediment <- sed
rm(a, sed)

# Delete the tif file because it is very big
file.remove(here::here("data-raw", "sediments", "sed_plume_avg.tif"))



# 6. WATER POLLUTION: NITROGEN
# This layer was produced in this work
# (code to produce it from raw data will be uploaded later, when Amelia Wenger comes back from maternity leave)

# First unzip the nit_plume_avg.zip file in local-reef-pressures\data-raw\nitrogen into the same folder
unzip(here("data-raw", "nitrogen", "nit_plume_avg.zip"), exdir = here("data-raw", "nitrogen"))
a <- raster::raster(here("data-raw", "nitrogen", "nit_plume_avg.tif"))
a.ton <- a / 1000 # Converting kg to tons

# Get centroids and reproject them in the same CRS of the raster (Mollweide)
# You do not need to run the two following lines if you have already done it for the sediment layer,
# Because the sediment and the nitrogen layers are in the same CRS (Mollweide)
# allreefs_centroids <- rgeos::gCentroid(allreefs, byid=T)
# allreefs_centroids_Mol <- spTransform(allreefs_centroids, sp::CRS(proj4string(a.ton)))

# Read (extract) the values of the nitrogen layer corresponding to the centroids of the reef cells
# And store it into the nit vector
nit <- raster::extract(a.ton, allreefs_centroids_Mol)

# Look at the values
hist(nit, plot = F) # Some very high values!

# Add the nit vector to the allreefs layer
allreefs$nutrient <- nit
rm(a, a.ton, nit)

# Delete the tif file because it is very big
file.remove(here::here("data-raw", "nitrogen", "nit_plume_avg.tif"))



# Clear the memory from unnecessary objects
rm(allreefs_buffer, allreefs_centroids, allreefs_centroids_Mol, EPSG, new_proj)

# Set vthreats vector, containing the name of the six pressures
vthreats <- c(
  "grav_NC",
  "pop_count",
  "num_ports",
  "reef_value",
  "sediment",
  "nutrient"
)

# Remove unnecessary column from allreefs: reef percentage
allreefs@data$reefpct <- NULL

# Convert it to sf
allreefs <- sf::st_as_sf(allreefs)

# Join bcus
load(here::here("data-raw", "50-reefs", "bcus.RData"))
allreefs_withBCU <- left_join(allreefs, bcus, by = "OBJECTID")

# Make a column saying whether the reef cell is in a BCU or not
allreefs_withBCU$is.bcu <- "non BCUs"
allreefs_withBCU$is.bcu[!is.na(allreefs_withBCU$ReefName)] <- "BCUs"

# Change the name of column ReefName to BCU_name
allreefs_withBCU <- dplyr::mutate(allreefs_withBCU, BCU_name = ReefName)
allreefs_withBCU$ReefName <- NULL
rm(bcus)

# Define new columns containing the raw pressure values and name them xxx_raw
allreefs_withBCU %>% dplyr::mutate(
  grav_NC_raw = grav_NC,
  pop_count_raw = pop_count,
  num_ports_raw = num_ports,
  reef_value_raw = reef_value,
  sediment_raw = sediment,
  nutrient_raw = nutrient
) -> allreefs_withBCU_prc

# Define a function that calculates percentiles
# and set values to 0 where the original values are zero
# This is needed as the ecdf function in R sets ties to the highest percentile
calc.percentiles <- function(data.in, indicator) {
  y <- ecdf(data.in[[indicator]])(data.in[[indicator]])
  id.0 <- which(data.in[[indicator]] == 0)
  y[id.0] <- 0
  y
}

# Calculate percentile of each of the six pressures and store them in the original columns
for (indicator in vthreats) {
  allreefs_withBCU_prc[[indicator]] <- calc.percentiles(allreefs_withBCU_prc, paste0(indicator, "_raw"))
}

# Store percentiles into dataframe "threats"
threats <- as.data.frame(allreefs_withBCU_prc)[, vthreats]

# Use the"perceived threat level" from Wear 2016 Mar Pol (originally on a scale from 0 to 6)
# to define weights for each of the six pressures
# 4.3 # Overfishing: gravity
# 4.3 # Coastal development; pop_count, num_ports, reef value
# 3.8 # Watershed pollution; sediment, nutrient
# 3.7 # Thermal stress;
# 3.2 # Marine pollution;
# 3.0 # Ocean acidification;
# Thus for us:
vweights <- c(4.3, 4.3, 4.3, 4.3, 3.8, 3.8)
names(vweights) <- vthreats

# Calculate the cumulative impact score as a weighted mean of the six pressures (percentiles)
cumul_score <- matrixStats::rowWeightedMeans(as.matrix(threats), w = vweights, na.rm = T)

# Calculate the cumulative impact score as a unweighted mean for comparison
cumul_score_uw <- rowMeans(threats, na.rm = T)

# Compare wighted and unweighted mean and see that they are very strongly correlated
plot(cumul_score, cumul_score_uw)
hist(cumul_score)
hist(cumul_score_uw)
cor.test(cumul_score, cumul_score_uw, method = "spearman")

# Store the cumulative impact score in the allreefs_with_BCU_prc layer
allreefs_withBCU_prc$cumul_score <- cumul_score
rm(cumul_score_uw)

# Calculate top threat
allreefs_withBCU_prc$top_threat <- apply(as.matrix(threats), 1, which.max)

# Add region
load(here::here("data-raw", "regions", "allreefs_withRegion.RData"))
allreefs_withBCU_prc$Region <- allreefs_withRegion$Region
rm(allreefs_withRegion)

# Reorder columns and change name of the layer
allreefs <- allreefs_withBCU_prc[, c(1, 8:13, 23:25, 15:16, 17:22, 2:7)]
rm(allreefs_withBCU, allreefs_withBCU_prc)

# Save final dataset in three formats
save(allreefs, file = here::here("data", "allreefs.RData"))
rgdal::writeOGR(as_Spatial(allreefs), here::here("data"), "allreefs", "ESRI Shapefile")
sf::st_write(allreefs, dsn = paste0(getwd(), "/data/allreefs.gpkg"), driver = "GPKG")



# SENSITIVITY ANALYSIS OF THE COASTAL DEVELOPMENT LAYER TO BUFFER SIZE
# Keep only useful columns and Reconvert allreefs back to SpatialPolygonsDataFrame
allreefs %>%
  select("OBJECTID", "pop_count_raw") %>%
  as_Spatial() -> allreefs_sp

# Reload population count layer, convert raster object to point shapefile
a <- raster::raster(here::here("data-raw", "coastal-development", "gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_2pt5_min.tif"))
p <- raster::rasterToPoints(a, spatial = T)
names(p) <- "values"

# and convert it into the project CRS (Mercator centered on meridian 150E)
EPSG <- rgdal::make_EPSG()
id <- which(EPSG$code == 3832)
new_proj <- EPSG$prj4[id]
p <- spTransform(p, sp::CRS(new_proj))
rm(a)

# Define alternative buffer sizes (1 to 100 km)
a_buffer_1 <- gBuffer(allreefs_sp, byid = T, width = 1000)
a_buffer_10 <- gBuffer(allreefs_sp, byid = T, width = 10000)
a_buffer_15 <- gBuffer(allreefs_sp, byid = T, width = 15000)
a_buffer_20 <- gBuffer(allreefs_sp, byid = T, width = 20000)
a_buffer_50 <- gBuffer(allreefs_sp, byid = T, width = 50000)
a_buffer_100 <- gBuffer(allreefs_sp, byid = T, width = 100000)

# Calculate pop count by tile (doing it on the entire SpatialPolygons might crash)
allreefs_sp$pop_count_1 <- IntersectByTile(a_buffer_1, p, step = 1e6)
allreefs_sp$pop_count_10 <- IntersectByTile(a_buffer_10, p, step = 1e6)
allreefs_sp$pop_count_15 <- IntersectByTile(a_buffer_15, p, step = 1e6)
allreefs_sp$pop_count_20 <- IntersectByTile(a_buffer_20, p, step = 1e6)
allreefs_sp$pop_count_50 <- IntersectByTile(a_buffer_50, p, step = 1e6)
allreefs_sp$pop_count_100 <- IntersectByTile(a_buffer_100, p, step = 1e6)

# Rename the raw column to 5km, and extract the useful columns
pop_count_raw <-
  mutate(allreefs_sp@data,
    pop_count_5 = pop_count_raw
  ) %>%
  select(
    pop_count_1,
    pop_count_5,
    pop_count_10,
    pop_count_15,
    pop_count_20,
    pop_count_50,
    pop_count_100
  )

# Define a new dataframe that will contain the percentile values
pop_count <- pop_count_raw

# Calculate percentiles
for (i in 1:7) {
  y <- ecdf(pop_count_raw[, i])(pop_count_raw[, i])
  id.0 <- which(pop_count_raw[, i] == 0)
  y[id.0] <- 0
  pop_count[, i] <- y
}

# Change names
names(pop_count) <- c("1 km", "5 km", "10 km", "15 km", "20 km", "50 km", "100 km")

# Calculate Spearman correlations and draw the correlogram
corrgram(pop_count,
  lower.panel = NULL,
  upper.panel = panel.cor,
  cor.method = "spearman"
)
# Then saved as Figure S12
