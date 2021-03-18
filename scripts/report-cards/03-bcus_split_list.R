# Split BCUs into a list

library(sf)
library(dplyr)
library(purrr)

# Read in BCUs
load(here::here("data", "allreefs.RData"))
bcus_prefs <- readRDS(here::here("data", "report-cards", "bcus_prefs.rds"))

# Filter for BCUs, rename BCU_name to bcu, and keep raw columns only
bcus <- allreefs %>%
  filter(is.bcu == "BCUs") %>%
  select(bcu = BCU_name, num_ports = num_ports_raw, grav_NC = grav_NC_raw, pop_count = pop_count_raw, reef_value = reef_value_raw, sediment = sediment_raw, nutrient = nutrient_raw, score, scoreth, scorepfc, scorecy, scoretr, scorecn, geom)

rm(allreefs)

bcus <- bcus %>%
  left_join(bcus_prefs %>%
    bind_rows() %>%
    distinct(bcu, crs), by = "bcu")

# Split into list
bcus_list <- bcus %>%
  split(.$bcu)

# Set CRS
bcus_list <- map(
  bcus_list,
  function(.x) {
    crs <- unique(.x[["crs"]])
    if (crs == 3823) {
      .x
    } else {
      st_transform(.x, crs = crs)
    }
  }
)

saveRDS(bcus_list, here::here("data", "report-cards", "bcus_list.rds"))
