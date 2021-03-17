library(dplyr)

bcus_wcs_maps_raw <- readr::read_csv(here::here("data", "report-cards", "raw", "bcus_wcs_maps.csv"))

bcus_wcs_maps <- bcus_wcs_maps_raw %>%
  filter(!is.na(wcs_map))

saveRDS(bcus_wcs_maps, here::here("data", "report-cards", "bcus_wcs_maps.rds"))
