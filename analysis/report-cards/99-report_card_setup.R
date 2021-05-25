library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(sf)
library(forcats)
library(tmap)
library(grid)
library(stringr)
library(ggbeeswarm)
library(glue, warn.conflicts = FALSE)

source(here::here("R", "map_indicator.R"))
source(here::here("R", "plot_indicator.R"))
source(here::here("R", "square_bbox.R"))
source(here::here("R", "recode_bcus.R"))
source(here::here("R", "plot_pressure_ranking_percentiles.R"))

theme_set(theme_minimal(10))

bcus <- readRDS(here::here("data", "report-cards", "bcus_list.rds"))
bcus_tiles_indicator <- readRDS(here::here("data", "report-cards", "bcus_tiles_indicator.rds"))
bcus_ext <- readRDS(here::here("data", "report-cards", "bcus_ext.rds"))

top_pressures <- readRDS(here::here("data", "report-cards", "bcus_top_pressures.rds"))
bcus_percentiles <- readRDS(here::here("data", "report-cards", "bcus_percentiles.rds"))

bcus_regions <- readRDS(here::here("data", "report-cards", "bcus_regions.rds"))
bcus_prefs <- readRDS(here::here("data", "report-cards", "bcus_prefs.rds"))

bcus_wcs_maps <- readRDS(here::here("data", "report-cards", "bcus_wcs_maps.rds"))
