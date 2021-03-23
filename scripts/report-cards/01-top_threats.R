# Calculate all reefs medians, BCU medians, percentiles, and rank top threats

library(sf)
library(dplyr)
library(tidyr)
library(stringr)

load(here::here("data", "allreefs.RData"))

bcus_raw <- allreefs %>%
  filter(is.bcu == "BCUs")

all_reefs_raw <- allreefs

rm(allreefs)

# Convert to tibble
bcus <- bcus_raw %>%
  as_tibble() %>%
  rename(bcu = BCU_name)

all_reefs <- all_reefs_raw %>%
  as_tibble()

# Flip climate scores so that lower is "better"
bcus <- bcus %>%
  mutate_at(vars(contains("score")), ~ -1 * .x)

all_reefs <- all_reefs %>%
  mutate_at(vars(contains("score")), ~ -1 * .x)

# Take indicators, transform to long, compute medians
bcus_medians <- bcus %>%
  select(bcu, num_ports_raw, grav_NC_raw, pop_count_raw, reef_value_raw, sediment_raw, nutrient_raw, score, scoreth, scorepfc, scorecy, scoretr, scorecn) %>%
  pivot_longer(
    cols = -bcu,
    names_to = "indicator",
    values_to = "value"
  ) %>%
  group_by(bcu, indicator) %>%
  summarise(
    reef_median = median(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(indicator = str_remove_all(indicator, "_raw"))

# Calculate median for all reefs
all_reefs_medians <- all_reefs %>%
  as_tibble() %>%
  select(num_ports_raw, grav_NC_raw, pop_count_raw, reef_value_raw, sediment_raw, nutrient_raw, score, scoreth, scorepfc, scorecy, scoretr, scorecn) %>%
  pivot_longer(
    cols = everything(),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  group_by(indicator) %>%
  summarise(
    median = median(value, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(indicator = str_remove_all(indicator, "_raw"))

# Combine data
bcus_all_reefs_medians <- bcus_medians %>%
  left_join(all_reefs_medians, by = "indicator") %>%
  mutate(indicator_label = recode(indicator,
    grav_NC = "Fishing: Market Pressure",
    pop_count = "Coastal Development: Human Population",
    num_ports = "Industrial Development: Ports",
    reef_value = "Tourism: Reef Value",
    sediment = "Pollution: Sedimentation",
    nutrient = "Pollution: Nitrogen",
    score = "Climate: Composite Score",
    scoreth = "Climate: Thermal History",
    scorepfc = "Climate: Thermal Future",
    scorecy = "Climate: Cyclone Risk",
    scoretr = "Climate: Recent Stress",
    scorecn = "Climate: Connectivity"
  ))

# Get percentiles for each value

bcus_percentiles <- bcus %>%
  as_tibble() %>%
  select(bcu, num_ports_raw, grav_NC_raw, pop_count_raw, reef_value_raw, sediment_raw, nutrient_raw, score, scoreth, scorepfc, scorecy, scoretr, scorecn) %>%
  dplyr::mutate(dplyr::across(-bcu, function(x) {
    percentile <- ecdf(all_reefs[[dplyr::cur_column()]])(x)
    percentile[which(x == 0)] <- 0
    percentile
  })) %>%
  tidyr::pivot_longer(
    cols = -bcu,
    names_to = "indicator",
    values_to = "percentile"
  ) %>%
  mutate(indicator = str_remove_all(indicator, "_raw"))

# Check that derived percentiles are the same as from the .RData file
# Use mean, median, and number of distinct values as summary statistics

rdata_percentiles_summary <- bcus %>%
  select(bcu, num_ports, grav_NC, pop_count, reef_value, sediment, nutrient) %>%
  pivot_longer(cols = -bcu, names_to = "indicator", values_to = "value") %>%
  group_by(bcu, indicator) %>%
  summarise(across(c(value),
    .fns = c(
      mean = ~ mean(.x, na.rm = TRUE),
      median = ~ median(.x, na.rm = TRUE),
      n = ~ n_distinct(.x, na.rm = TRUE)
    ),
    .names = "{.fn}"
  ),
  .groups = "drop"
  ) %>%
  pivot_longer(cols = c(mean, median, n), names_to = "measure", values_to = "value")

bcus_percentiles_summary <- bcus_percentiles %>%
  filter(!str_starts(indicator, "score")) %>%
  group_by(bcu, indicator) %>%
  summarise(across(c(percentile),
    .fns = c(
      mean = ~ mean(.x, na.rm = TRUE),
      median = ~ median(.x, na.rm = TRUE),
      n = ~ n_distinct(.x, na.rm = TRUE)
    ),
    .names = "{.fn}"
  ),
  .groups = "drop"
  ) %>%
  pivot_longer(cols = c(mean, median, n), names_to = "measure", values_to = "value")

rdata_percentiles_summary %>%
  full_join(bcus_percentiles_summary,
    by = c("bcu", "indicator", "measure"),
    suffix = c("_rdata", "_derived")
  ) %>%
  mutate(values_equal = value_rdata == value_derived) %>%
  pull(values_equal) %>%
  all()

# Looks good!

# Get median percentile for each indicator
median_percentile <- bcus_percentiles %>%
  dplyr::group_by(bcu, indicator) %>%
  dplyr::summarise(percentile = median(percentile, na.rm = TRUE), .groups = "drop")

# Rank top threats based on median percentile, excluding Climate indicators and number of ports
# To break any ties (e.g. multiple with percentile is 0), use the mean and favor larger mean values
# Just so it doesn't look weird in the report cards
bcus_means <- bcus %>%
  select(bcu, num_ports_raw, grav_NC_raw, pop_count_raw, reef_value_raw, sediment_raw, nutrient_raw, score, scoreth, scorepfc, scorecy, scoretr, scorecn) %>%
  pivot_longer(
    cols = -bcu,
    names_to = "indicator",
    values_to = "value"
  ) %>%
  group_by(bcu, indicator) %>%
  summarise(
    reef_mean = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(indicator = str_remove_all(indicator, "_raw"))

top_threats_indicators <- median_percentile %>%
  left_join(bcus_means, by = c("bcu", "indicator")) %>%
  filter(!str_detect(indicator, "score") & indicator != "num_ports") %>%
  dplyr::group_by(bcu) %>%
  dplyr::arrange(dplyr::desc(percentile), dplyr::desc(reef_mean)) %>%
  dplyr::mutate(rank = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select(bcu, indicator, rank)

# Combine
top_threats <- bcus_all_reefs_medians %>%
  left_join(median_percentile, by = c("bcu", "indicator")) %>%
  left_join(top_threats_indicators, by = c("bcu", "indicator"))

# Split into list
top_threats <- top_threats %>%
  split(.$bcu)

saveRDS(top_threats, here::here("data", "report-cards", "bcus_top_threats.rds"))

bcus_percentiles <- bcus_percentiles %>%
  split(.$bcu)

saveRDS(bcus_percentiles, here::here("data", "report-cards", "bcus_percentiles.rds"))
