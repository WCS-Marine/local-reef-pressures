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

# PREPARE FINAL DATASET

vthreats <- c("grav_NC",
              "pop_count",
              "num_ports",
              "reef_value",
              "sediment",
              "nutrient")

# Remove unnecessary column: reef percentage
allreefs@data$reefpct <- NULL

# Convert it to sf
allreefs <- st_as_sf(allreefs)

# Join bcus
bcus <- read_sf(here::here("data"),"bcus") # Used only to retrieve bcu name and id, not for threat values
allreefs_withBCU <- left_join(allreefs, select(as.data.frame(bcus),"OBJECTID","ReefName"), by="OBJECTID")
allreefs_withBCU$is.bcu <- "non BCUs"
allreefs_withBCU$is.bcu[!is.na(allreefs_withBCU$ReefName)] <- "BCUs"
allreefs_withBCU <- mutate(allreefs_withBCU, BCU_name=ReefName)
allreefs_withBCU$ReefName <- NULL
rm(bcus)

# Calculate percentiles
allreefs_withBCU %>% mutate(grav_NC_raw = grav_NC,
                            pop_count_raw = pop_count,
                            num_ports_raw = num_ports,
                            reef_value_raw = reef_value,
                            sediment_raw = sediment,
                            nutrient_raw = nutrient) -> allreefs_withBCU_prc

calc.percentiles <- function(data.in, indicator) {
  y <- ecdf(data.in[[indicator]])(data.in[[indicator]])
  id.0 <- which(data.in[[indicator]] == 0)
  y[id.0] <- 0
  y
}
for (indicator in vthreats) {
  allreefs_withBCU_prc[[indicator]] <- calc.percentiles(allreefs_withBCU_prc, paste0(indicator,"_raw"))
}

# Store normalized values into dataframe "threats"
threats <- as.data.frame(allreefs_withBCU_prc)[,vthreats]

# Calculate cumulative score
# Using "perceived threat level" from Wear 2016 Mar Pol, on a scale from 0 to 6
# 4.3 # Overfishing: gravity
# 4.3 # Coastal development; pop_count, num_ports, reef value
# 3.8 # Watershed pollution; sediment, nutrient
# 3.7 # Thermal stress;
# 3.2 # Marine pollution;
# 3.0 # Ocean acidification;
# Thus for us:
vweights <- c(4.3, 4.3 ,4.3, 4.3, 3.8, 3.8)
names(vweights) <- vthreats
cumul_score <- rowWeightedMeans(as.matrix(threats), w = vweights, na.rm = T) # Weighted mean
cumul_score_uw <- rowMeans(threats, na.rm = T) # Unweighted mean for comparison
plot(cumul_score, cumul_score_uw)
hist(cumul_score)
hist(cumul_score_uw)
cor.test(cumul_score, cumul_score_uw, method="spearman")
allreefs_withBCU_prc$cumul_score <- cumul_score
rm(cumul_score_uw)

# Calculate top threat
allreefs_withBCU_prc$top_threat <- apply(as.matrix(threats), 1, which.max)

# Add region
load(here::here("data-raw","regions","allreefs_withRegion.RData"))
allreefs_withBCU_prc$Region <- allreefs_withRegion$Region
rm(allreefs_withRegion)

# Change name
allreefs <- allreefs_withBCU_prc
rm(allreefs_withBCU, allreefs_withBCU_prc)

# Save final dataset
save(allreefs, file=here::here("data","allreefs.RData"))
writeOGR(allreefs, here::here("data"), "allreefs", "ESRI Shapefile")
st_write(allreefs, dsn = paste0(getwd(),"/data/allreefs.gpkg"), driver="GPKG")



