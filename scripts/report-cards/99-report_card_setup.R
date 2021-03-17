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

source(here::here("helper-scripts", "map_indicator.R"))
source(here::here("helper-scripts", "plot_indicator.R"))
source(here::here("helper-scripts", "square_bbox.R"))
source(here::here("helper-scripts", "recode_bcus.R"))
source(here::here("helper-scripts", "plot_threat_ranking_percentiles.R"))

theme_set(theme_minimal(10))

bcus <- readRDS(here::here("data", "report-cards", "bcus_list.rds"))
bcus_tiles_indicator <- readRDS(here::here("data", "report-cards", "bcus_tiles_indicator.rds"))
bcus_ext <- readRDS(here::here("data", "report-cards", "bcus_ext.rds"))

top_threats <- readRDS(here::here("data", "report-cards", "bcus_top_threats.rds"))
bcus_percentiles <- readRDS(here::here("data", "report-cards", "bcus_percentiles.rds"))

bcus_regions <- readRDS(here::here("data", "report-cards", "bcus_regions.rds"))
bcus_prefs <- readRDS(here::here("data", "report-cards", "bcus_prefs.rds"))

bcus_wcs_maps <- readRDS(here::here("data", "report-cards", "bcus_wcs_maps.rds"))
