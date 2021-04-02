# Prepare datasets

rm(list=ls())

library(sf)
library(tidyverse)
library(here)
library(matrixStats)

# library(tmaptools)


# Threat names used in allreefs as column names
vthreats <- c("grav_NC",
              "pop_count",
              "num_ports",
              "reef_value",
              "sediment",
              "nutrient")

# Read shapefiles
countries <- read_sf(paste0(getwd(),"/../reef-context/data-raw/natural-earth/cultural"),"ne_10m_admin_0_countries")
allreefs <- read_sf(paste0(getwd(),"/../reef-context/data"),"allreefs")
allreefsWGS84 <- read_sf(here::here("data"),"allreefsWGS84")
bcus <- read_sf(paste0(getwd(),"/../reef-context/data"),"bcus") # Used only to retrieve bcu name and id, not for threat values
zones <- read_sf("C:/Users/Marco/Desktop/Intersect_EEZ_IHO_v4_2020","Intersect_EEZ_IHO_v4_2020") # Flanders Marine Institute (2020). The intersect of the Exclusive Economic Zones and IHO sea areas, version 4. Available online at https://www.marineregions.org/.https://doi.org/10.14284/402


# PREPARE ALLREEFS DATASET

# Remove unnecessary columns
allreefs_onlyLocal <- allreefs %>% select(OBJECTID, grav_NC, pop_count, num_ports, reef_value, sediment, nutrient, geometry)

# Join bcus
allreefs_withBCU <- left_join(allreefs_onlyLocal, select(as.data.frame(bcus),"OBJECTID","ReefName","COUNTRY"), by="OBJECTID")
allreefs_withBCU$is.bcu <- "non BCUs"
allreefs_withBCU$is.bcu[!is.na(allreefs_withBCU$ReefName)] <- "BCUs"

# Intersect allreefsWGS84 with the (EEZ-IHO) intersection
a <- st_join(allreefsWGS84, zones, largest = T) # takes about 70 minutes

# Join marine regions and EEZ
# a1 <- select(a,"MRGID","MARREGION","MRGID_IHO","IHO_SEA","MRGID_EEZ","EEZ","MRGID_TER1","TERRITORY1")
allreefs_withBCU$MRGID_IHO <- as.data.frame(a)[,c("MRGID_IHO")]
allreefs_withBCU$IHO_SEA <- as.data.frame(a)[,c("IHO_SEA")]
allreefs_withBCU$MRGID_TER1 <- as.data.frame(a)[,c("MRGID_TER1")]
allreefs_withBCU$TERRITORY1 <- as.data.frame(a)[,c("TERRITORY1")]

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
plot(score.l, score.e)
hist(score.l)
hist(score.e)
cor.test(score.l, score.e, method="spearman")

# Calculate top threat
allreefs_withBCU_prc_centroids$top.threat <- apply(as.matrix(threats), 1, which.max)

# We will use polygons to make the Maps, so
allreefs_withBCU_prc$top.threat <- allreefs_withBCU_prc_centroids$top.threat
allreefs_withBCU_prc$score.l <- allreefs_withBCU_prc_centroids$score.l
allreefs_withRegion <- read_sf(paste0(getwd(),"/data"),"allreefs_withRegion")
allreefs_withBCU_prc$Region <- allreefs_withRegion$REEFTHR
allreefs_withBCU_prc_centroids$Region <- allreefs_withRegion$REEFTHR
rm(allreefs_withRegion)

# PREPARE COUNTRY SHAPEFILE

# Change CRS for countries to match allreefs
# countries <- countries[-c(173, 175),]
countries_proj <- st_transform(countries, st_crs(allreefs_withBCU_prc))

# Crop countries to bbox of allreefs
allreefs_bbox_extended <- st_bbox(allreefs_withBCU_prc)
allreefs_bbox_extended[4] <- 4.5e6
countries_eq <- st_crop(countries_proj, allreefs_bbox_extended)

# SAVE

save(allreefs_withBCU_prc,
     allreefs_withBCU_prc_centroids,
     countries_eq,
     threats,
     vthreats,
     file="DataForAnalysis.RData")

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
save(allreefs_withBCU_prc, file=paste0(getwd(),"/data/allreefs.RData"))
