# Prepare datasets

rm(list=ls())

library(sf)
library(tidyverse)
library(here)
library(matrixStats)

# Threat names used in allreefs as column names
vthreats <- c("grav_NC",
              "pop_count",
              "num_ports",
              "reef_value",
              "sediment",
              "nutrient")

# Read shapefiles
countries <- read_sf(here::here("data-raw","natural-earth","cultural"),"ne_10m_admin_0_countries")
allreefs <- read_sf(here::here("data"),"allreefs")
bcus <- read_sf(here::here("data"),"bcus") # Used only to retrieve bcu name and id, not for threat values





# PREPARE ALLREEFS DATASET

# Remove unnecessary columns
allreefs_onlyLocal <- allreefs %>% select(OBJECTID, grav_NC, pop_count, num_ports, reef_value, sediment, nutrient, geometry)

# Join bcus
allreefs_withBCU <- left_join(allreefs_onlyLocal, select(as.data.frame(bcus),"OBJECTID","ReefName","COUNTRY"), by="OBJECTID")
allreefs_withBCU$is.bcu <- "non BCUs"
allreefs_withBCU$is.bcu[!is.na(allreefs_withBCU$ReefName)] <- "BCUs"

# Calculate percentiles
allreefs_withBCU %>% mutate(grav_NC_raw = grav_NC,
                            pop_count_raw = pop_count,
                            num_ports_raw = num_ports,
                            reef_value_raw = reef_value,
                            sediment_raw = sediment,
                            nutrient_raw = nutrient) -> allreefs_withBCU_prc
allreefs_withBCU_prc

calc.percentiles <- function(data.in, indicator) {
  y <- ecdf(data.in[[indicator]])(data.in[[indicator]])
  id.0 <- which(data.in[[indicator]] == 0)
  y[id.0] <- 0
  y
}
for (indicator in vthreats) {
  allreefs_withBCU_prc[[indicator]] <- calc.percentiles(allreefs_withBCU_prc, paste0(indicator,"_raw"))
}

save(allreefs_withBCU_prc,
     vthreats,
     file="PolygonsDataset.RData")

# Get centroids of allreefs
allreefs_withBCU_prc_centroids <- st_centroid(allreefs_withBCU_prc)


# CUMULATIVE IMPACT SCORE AND TOP THREAT

# Store normalized values into dataframe "threats"
threats <- as.data.frame(allreefs_withBCU_prc_centroids)[,vthreats]

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
score.l <- rowWeightedMeans(as.matrix(threats),
                            w = vweights, na.rm = T) # Weighted mean
score.e <-         rowMeans(threats,
                            na.rm = T) # Unweighted mean for comparison
allreefs_withBCU_prc_centroids$score.l <- score.l

# Calculate top threat and tertiles
allreefs_withBCU_prc_centroids$top.threat <- apply(as.matrix(threats), 1, which.max)
# d <- list()
# for (i in 1 : 6) {
#   tertiles <- quantile(threats[,i], seq(0,1,1/3), na.rm=T)
#   # "cut" requires unique breaks. This adjusts the breaks without changing the results:
#   if(i == 3 | i == 4) tertiles <- c(0,0.01,0.02,1)
#   if(i == 5 | i == 6) tertiles[1:2] <- c(0,1e-12)
#   d[[i]] <- cut(threats[,i],
#                 tertiles,
#                 include.lowest=T,
#                 labels=c(1,2,3))
# }
# # To inspect low values
# # sort(threats[(threats[,i] > 0),i])
# names(d) <- paste0(vthreats,"_tertile")
# # Bind the columns with the tertiles together, then with the dataframe of allreefs_centroids
# dd <- cbind.data.frame(d)
# allreefs_withBCU.norm_centroids <- dplyr::bind_cols(allreefs_withBCU.norm_centroids,dd)


# PREPARE COUNTRY SHAPEFILE

# Change CRS for countries to match allreefs
# countries <- countries[-c(173, 175),]
countries_proj <- st_transform(countries, st_crs(allreefs))

# Crop countries to bbox of allreefs
allreefs_bbox_extended <- st_bbox(allreefs)
allreefs_bbox_extended[4] <- 4.5e6
countries_eq <- st_crop(countries_proj, allreefs_bbox_extended)



# SAVE

save(allreefs_withBCU_prc_centroids,
     countries_eq,
     threats,
     vthreats,
     file="DataForAnalysis.RData")
