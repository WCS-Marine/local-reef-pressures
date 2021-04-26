# Get tiles for BCUs

library(purrr)
library(tmaptools)
library(dplyr)
library(ggspatial)

source(here::here("helper-scripts", "square_bbox.R"))
source(here::here("helper-scripts", "get_left_right_tiles.R"))

bcus <- readRDS(here::here("data", "report-cards", "bcus_list.rds"))
bcus_prefs <- readRDS(here::here("data", "report-cards", "bcus_prefs.rds"))

# Set how much to enlarge the bounding boxes, for each BCU
bcus_ext <- bcus_prefs %>%
  map("ext")

saveRDS(bcus_ext, here::here("data", "report-cards", "bcus_ext.rds"))

# Get bing tiles (indicator maps)
bcus_tiles_indicator <- map2(bcus[which(!names(bcus) %in% c("Vanua Balavu - NE Fiji", "Vatu-i-Ra"))], bcus_ext[which(!names(bcus_ext) %in% c("Vanua Balavu - NE Fiji", "Vatu-i-Ra"))], ~ read_osm(square_bbox(.x, ext = .y), type = "bing"))

fiji_tiles_indicator <- list(
  `Vanua Balavu - NE Fiji` = get_left_right_tiles(bcus, "Vanua Balavu - NE Fiji", bcus_ext[["Vanua Balavu - NE Fiji"]]),
  `Vatu-i-Ra` = get_left_right_tiles(bcus, "Vatu-i-Ra", bcus_ext[["Vatu-i-Ra"]]))

bcus_tiles_indicator <- append(bcus_tiles_indicator, fiji_tiles_indicator)

saveRDS(bcus_tiles_indicator, here::here("data", "report-cards",  "bcus_tiles_indicator.rds"))
