# Plot map
# Working simplified script for Sharla, based on Plot Figure 1 and 2
# Marco Andrello
# 15/01/2021

rm(list=ls())

library(tidyverse)
library(sf)
library(here)
library(tmap)
library(RColorBrewer)
library(ggplot2)
library(scales)

load(here("scripts","DataForAnalysis.RData"))
# Contains:
class(allreefs_withBCU_prc_centroids); names(allreefs_withBCU_prc_centroids)
# shapefile (sf format) of reef centroids containing all the data (threats, countries, etc)
# Note: TERRITORY is preliminary
# _raw indicates raw threat value; the others are percentile-transformed
# score.l is the cumulative score
class(countries_eq)
# shapefile (sf format) of country borders trimmed to the coral reef distribution area
head(threats)
# values of threats (percentile-transformed) in data.frame format
vthreats
# vector of names of the 6 threats as they appear in the R objects 

# Plot countries at global scale
m <- tm_shape(countries_eq) +
  tm_polygons(col="gray",
              border.col="darkgray",
              lwd=0.2)


# Set up breaks for the colorscale of the 6 threats and the cumulative score
breaks <- list()
for (i in 1 : 7) breaks[[i]] <- seq(0,1,0.25)
names(breaks) <- c(vthreats,"score.l")


# Define margins of panel regions
bbox_panel <- st_bbox(countries_eq)
# Sets the longitude and latitude of the lower-left corner of the bbox of each region:
# (Remeber the Coordinate Reference System is in meters starting from longtitude 150 degrees East and the equator,
# so for example longitude 13e6 means 13000 km east of the 150 degree meridian and latitude 2e6 means 2000 km North of the Equator) 
# Define as a tribble (a tibble created by Row instead) so that we can include the names of each panel
v.bbox.panel <- tribble(
  ~xmin, ~ymin, ~xmax, ~ymax, ~name,
  -13e6, 1.1e6, NA, NA, "Middle East and North Africa",
  -13e6, -3e6, NA, NA, "East Africa",
  -9e6, -1e6, NA, NA, "Indian Ocean",
  -6e6, -1.3e6, NA, NA, "Coral Triangle",
  13e6, 2e6, NA, NA, "Caribbean and Bahamas",
  -1e6, -3.5e6, NA, NA, "Australia and Melanes"
)
# Set the horizontal extent (longitudinal span) of the boxes:
horiz.extent <- rep(3e6,nrow(v.bbox.panel))
# Coral triangle and Australia and Melanesia have larger horizontal extents:
horiz.extent[c(4,6)] <- c(6.8e6,5e6)
# Factor shape, if 1 = square box, if <1 is rectangular box
# All boxes are square, except Coral triangle and Caribbean and Bahamas

# Add this to the existing data set
v.bbox.panel <- v.bbox.panel %>%
  mutate(factor_shape = ifelse(name %in% c("Coral Triangle", "Caribbean and Bahamas"), 0.5, 1))
# text.size.scalebar <- rep(0.25, 7); text.size.scalebar[5] <- 0.125

# We work on the cumulative impact score for simplicity
indicator <- "score.l"
indicator_title <- "Cumulative impact score"

# Set up scale, colors and legend elements
# (I took this from your code Sharla)
if(indicator == "num_ports") {
  indicator_breaks <- breaks[[indicator]]
  legend_labels <- as.character(0:2)
  legend_style <- "fixed"
} else {
  indicator_breaks <- breaks[[indicator]]
  breaks_midpoint <- ceiling(length(indicator_breaks)/2)
  breaks_midpoint_value <- indicator_breaks[[breaks_midpoint]]
  blank_breaks <- rep("", breaks_midpoint - 2)
  legend_labels <- c(min(indicator_breaks), blank_breaks, breaks_midpoint_value, blank_breaks, max(indicator_breaks))
  legend_style <- "cont"
}
indicator_breaks
legend_labels


# Plot the global map of the cumulative impact score
m.global.cumulative <- m +
  tmap::tm_shape(allreefs_withBCU_prc_centroids) +
  tm_squares(size = 1,
             col = indicator,
             palette = brewer.pal(length(indicator_breaks), "OrRd"),
             shape = 15,
             scale = 0.01,
             style = legend_style,
             breaks = indicator_breaks,
             title.col = indicator_title,
             labels = legend_labels,
             showNA = FALSE) +
  tmap::tm_legend(show = FALSE)

# Set the full plot width here, and then each smaller panel's height is 1/4 of that, and their width is either 1/4 (square panels) or 1/2 (rectangular panels)
full_width <- 18.25
tmap_save(m.global.cumulative, filename = here::here("plots/cumulative_global.png"), width = full_width, height = 4, units = "cm", dpi = 600)

# Plot the six regional panels
for (i in 1 : nrow(v.bbox.panel)) {
  factor_shape <- v.bbox.panel$factor_shape[i]
  
  # Define bbox of panel region: here is where horiz.extent and fact.shape come into play
  bbox_panel[1] <- v.bbox.panel$xmin[i]
  bbox_panel[2] <- v.bbox.panel$ymin[i]
  bbox_panel[3] <- bbox_panel[1] + horiz.extent[i]
  bbox_panel[4] <- bbox_panel[2] + (bbox_panel[3] - bbox_panel[1])*factor_shape

  # First plot the country borders
  m.p <- tm_shape(countries_eq, bbox = bbox_panel) +
  tm_polygons(col="gray",
              border.col="darkgray",
              lwd=0.2) +
  # Then the reef pixels
  tm_shape(allreefs_withBCU_prc_centroids, bbox = bbox_panel) +
    tm_squares(size = 1,
               col = indicator,
               palette = brewer.pal(length(indicator_breaks), "OrRd"),
               shape = 15,
               scale = 0.05*factor_shape,
               style = legend_style,
               breaks = indicator_breaks) +
    tmap::tm_legend(show = F)
  
  tmap_save(m.p, filename = here::here(glue::glue("plots/cumulative_panel_{v.bbox.panel$name[i]}.png")), width = ifelse(factor_shape == 1, full_width/4, full_width/2), height = full_width/4, units = "cm", dpi = 600)
}

# # Print regional maps of cumulative score on files
# for (i in 1 : 6) {
#   cat("plotting",i,"\n")
#   png(paste0("cumulative_panel",i,".png"), width = 4.3, height = 4.3, units = "cm", res = 600)
#   print(panel.list.cumulative[[i]])
#   dev.off()
# }

# Plot the legend
reefs_for_legend <- allreefs_withBCU_prc_centroids[c(1000:1010),] # Reduced dataset to speed up the plotting of the legend
breaks_logscale <- seq(0,1,0.25)
leg <- tmap::tm_shape(reefs_for_legend) +
  tm_squares(size = 1,
             col = indicator,
             palette = brewer.pal(length(indicator_breaks), "OrRd"),
             style = legend_style,
             breaks = indicator_breaks,
             title.col = indicator_title,
             labels = legend_labels,
             showNA = FALSE,
             legend.col.is.portrait = F) +
  tmap::tm_layout(legend.only = T, legend.position =c("center","center"))

tmap_save(leg, filename = here::here(glue::glue("plots/legend.png")), width = 4.3, height = 2, units = "cm", dpi = 600)

# Construct plot using magick

library(magick)
plot_global <- image_read(here::here("plots/cumulative_global.png"))

# Read panels in, in order intended, and save them to a list

panels <- c(
  "Middle East and North Africa",
  "Indian Ocean",
  "Coral Triangle",
  "East Africa",
  "Australia and Melanes",
  "Caribbean and Bahamas"
)

panel_list <- vector("list", length = length(panels))
names(panel_list) <- panels

for(i in names(panel_list)) {
  panel_list[[i]] <- image_read(here::here(glue::glue("plots/cumulative_panel_{i}.png")))
}

legend <- image_read(here::here("plots/legend.png"))

# Create a blank image
# Height is 2 * panels + global panel + legend
# Width is global panel

panel_height <- image_info(panel_list[[1]])[["height"]]
global_height <- image_info(plot_global)[["height"]]
global_width <- image_info(plot_global)[["width"]]
legend_height <- image_info(legend)[["height"]]
legend_width <- image_info(legend)[["width"]]

plot_width <- global_width
plot_height <- global_height + 2 * panel_height + legend_height

plot_image <- image_blank(width = plot_width, height = plot_height, color = "white")

# Add the first 3 panels

# Start with 0 width offset
width_offset <- 0
for (i in 1:3) {
  plot_image <- image_composite(plot_image, panel_list[[i]], offset = glue::glue("+{width_offset}+0"))
  # Then increment the offset by the width of the panel, so the next panel can use it
  width_offset <- width_offset + image_info(panel_list[[i]])[["width"]]
}

# Add the global plot

plot_image <- image_composite(plot_image, plot_global, offset = glue::glue("+0+{image_info(panel_list[[i]])[['height']]}"))

# Add the last 3 panels

# Start with 0 width offset
width_offset <- 0
for (i in 4:6) {
  height_offset <- image_info(panel_list[[i]])[["height"]] + image_info(plot_global)[["height"]]
  plot_image <- image_composite(plot_image, panel_list[[i]], offset = glue::glue("+{width_offset}+{height_offset}"))
  # Then increment the offset by the width of the panel, so the next panel can use it
  width_offset <- width_offset + image_info(panel_list[[i]])[["width"]]
}

# Add the legend

legend_height_offset <- plot_height - legend_height
legend_width_offset <- plot_width / 2 - legend_width / 2
plot_image <- image_composite(plot_image, legend, offset = glue::glue("+{legend_width_offset}+{legend_height_offset}"))

image_write(plot_image, here::here("plots/final_plot.png"))

                