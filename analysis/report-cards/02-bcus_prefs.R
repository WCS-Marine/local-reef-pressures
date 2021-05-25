# Set preferences for BCUs
# Including region for context map, extent of square bounding box (for smaller context map), and CRS

library(dplyr)
library(readr)

bcus_prefs <- read_csv(here::here("data", "report-cards", "raw", "bcus_prefs.csv"))
top_pressures <- readRDS(here::here("data", "report-cards", "bcus_top_pressures.rds"))

# Add CRS
bcus_prefs <- top_pressures %>%
  bind_rows() %>%
  distinct(bcu) %>%
  left_join(bcus_prefs, by = "bcu") %>%
  mutate(
    crs = case_when(
      bcu %in% c("Great Barrier Reef Central", "Mason - NGBR", "Eel - NGBR", "WhitsundayReef 1 - SGBR", "WhitsundayReef 2-SGBR", "Torres Strait") ~ 4326,
      bcu %in% c(
        "Tahiti", "Tuamotus Central", "Tuamotus Northern", "Tuamotus Southern",
        "Vanua Balavu - NE Fiji", "Vatu-i-Ra", "Solomon Islands", "Milne Bay"
      ) ~ 3460,
      TRUE ~ 3832
    ),
    zoom = case_when(
      bcu %in% c(
        "Great Barrier Reef Central",
        "Mason - NGBR",
        "Eel - NGBR",
        "Gallon-NGBR",
        "Mackay-GBR",
        "WhitsundayReef 1 - SGBR",
        "WhitsundayReef 2-SGBR"
      ) ~ 8,
      bcu %in% c(
        "Bahamas",
        "Belitung",
        "Bird’s Head",
        "Central Bahamas",
        "Central Sulawesi",
        "Gulf of Boni",
        "Mentawis",
        "Mindinao to Cebu",
        "Rakhine Coast",
        "Riau Islands",
        "Sabalana Islands",
        "SE Red Sea",
        "Singapore",
        "Southern Red Sea",
        "Sulu Archipelago",
        "SW Red Sea",
        "Cuba South",
        "Cuba Southeast",
        "Solomon Islands",
        "Banggai to Gulf of Tomini",
        "Southern Tanzania",
        "Tanzania/Kenya",
        "Cendrawasih",
        "Central Tanzania"
      ) ~ 6,
      bcu %in% c(
        "Cuba North/Bahamas",
        "Cuba Northwest",
        "Flores/Timor",
        "Hispanola",
        "North Sulawesi",
        "Sudan II",
        "Bird's Head",
        "Halmahera"
      ) ~ 5,
      TRUE ~ 7
    ),
    ext = coalesce(ext, 1.1)
  ) %>%
  split(.$bcu)

saveRDS(bcus_prefs, here::here("data", "report-cards", "bcus_prefs.rds"))
