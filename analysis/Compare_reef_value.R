# Compare spending value vs visitation value
library(here)
library(sf)
library(tidyverse)
library(corrgram)

load(here("data", "allspending.RData"))
load(here("data", "allreefs.RData"))

data <- data.frame(spending=allspending$reef_value,
                   visit=allreefs$reef_value)
cor.test(data$spending, data$visit, method="spearman")

top_threat <- data.frame(spending = allspending$top_threat,
                         visit = allreefs$top_threat,
                         equal = allspending$top_threat == allreefs$top_threat)
