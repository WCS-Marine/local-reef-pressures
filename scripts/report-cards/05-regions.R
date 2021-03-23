# Regions for BCUs

bcus_regions <- readr::read_csv(here::here("data", "report-cards", "raw", "bcu_country_region.csv"))
bcus_regions <- split(bcus_regions, bcus_regions$bcu)

saveRDS(bcus_regions, here::here("data", "report-cards", "bcus_regions.rds"))
