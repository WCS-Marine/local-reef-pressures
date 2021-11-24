# Code to
# - draw Figure 1 (map of top pressures)
# - draw Figure 2 (map of cumulative impact score)
# - Figure S2 to S7 (individual pressure maps)

# Marco Andrello
# 03/02/2021

rm(list = ls())

library(tidyverse)
library(sf)
library(here)
library(tmap)
library(RColorBrewer)
library(magick)
library(pdftools)

load(here::here("data", "allreefs.RData"))

# Names of the six pressures plus cumulative impact score
threat_names <- c(
  "Fishing", "Coastal pop", "Industrial dev",
  "Tourism", "Sediments", "Nitrogen", "Cumulative impact score"
)

# Names of the six pressure plus cumulative impact score in the columns of allreefs
vthreats <- c(
  "grav_NC", "pop_count", "num_ports",
  "reef_value", "sediment", "nutrient", "cumul_score"
)

# Panels zooming in six different geographical regions
panels <- c(
  "Middle East and North Africa",
  "Indian Ocean",
  "Coral Triangle",
  "East Africa",
  "Australia and Melanesia",
  "Caribbean and Bahamas"
)

# Set up breaks for the colorscale of the 6 threats and the cumulative score
breaks <- list()
for (i in 1:7) breaks[[i]] <- seq(0, 1, 0.25)
names(breaks) <- vthreats

# Read countries shapefile
countries <- sf::read_sf(here::here("data-raw", "natural-earth", "cultural"), "ne_10m_admin_0_countries")

# Change CRS for countries to match that of allreefs
countries_proj <- sf::st_transform(countries, sf::st_crs(allreefs))

# Crop countries layer to bbox of allreefs (and add some margin to ymax)
allreefs_bbox_extended <- sf::st_bbox(allreefs)
allreefs_bbox_extended[4] <- 4.5e6
countries_eq <- sf::st_crop(countries_proj, allreefs_bbox_extended)
rm(allreefs_bbox_extended, countries, countries_proj)

# Plot countries at global scale
m <- tmap::tm_shape(countries_eq) +
  tmap::tm_polygons(
    col = "gray",
    border.col = "darkgray",
    lwd = 0.2
  )

# Width of the figure
full_width <- 18.25

# Define margins of panel regions
bbox_panel <- sf::st_bbox(countries_eq)
# Sets the longitude and latitude of the lower-left corner of the bbox of each region:
# (Remeber the Coordinate Reference System is in meters starting from longtitude 150 degrees East and the equator,
# so for example longitude 13e6 means 13000 km east of the 150 degree meridian and latitude 2e6 means 2000 km North of the Equator)
# Define as a tribble (a tibble created by Row instead) so that we can include the names of each panel
v.bbox.panel <- tibble::tribble(
  ~xmin, ~ymin, ~xmax, ~ymax, ~name,
  -13e6, 1.1e6, NA, NA, "Middle East and North Africa",
  -13e6, -3e6, NA, NA, "East Africa",
  -9e6, -1e6, NA, NA, "Indian Ocean",
  -6e6, -1.3e6, NA, NA, "Coral Triangle",
  13e6, 2e6, NA, NA, "Caribbean and Bahamas",
  -1e6, -3.5e6, NA, NA, "Australia and Melanesia"
)
# Set the horizontal extent (longitudinal span) of the boxes:
horiz.extent <- rep(3e6, nrow(v.bbox.panel))
# Coral triangle and Australia and Melanesia have larger horizontal extents:
horiz.extent[c(4, 6)] <- c(6.8e6, 5e6)
# Factor shape, if 1 = square box, if <1 is rectangular box
# All boxes are square, except Coral triangle and Caribbean and Bahamas

# Add this to the existing data set
v.bbox.panel <- v.bbox.panel %>%
  dplyr::mutate(factor_shape = ifelse(name %in% c("Coral Triangle", "Caribbean and Bahamas"), 0.5, 1))


#############################################################################################
#  FIGURE 1: TOP THREAT
#############################################################################################
allreefs$top_threat <- factor(allreefs$top_threat)

# Top threat, global map
m_global_topthreats <- m +
  tmap::tm_shape(allreefs) +
  tmap::tm_fill(
    col = "top_threat",
    palette = "Set2"
  ) +
  tmap::tm_legend(show = FALSE)

# Save the global map
fs::dir_create(here::here("plots")) # Create plots folder if it doesn't exist
tmap::tmap_save(m_global_topthreats, filename = here::here(glue::glue("plots/topthreat_global.pdf")), width = full_width, height = 4, units = "cm")
rm(m_global_topthreats)

# Plot the six regional panels
for (i in 1:nrow(v.bbox.panel)) {
  cat("Plotting panel", i, "\n")

  # Define bbox of panel region: here is where horiz.extent and fact.shape come into play
  factor_shape <- v.bbox.panel$factor_shape[i]
  bbox_panel[1] <- v.bbox.panel$xmin[i]
  bbox_panel[2] <- v.bbox.panel$ymin[i]
  bbox_panel[3] <- bbox_panel[1] + horiz.extent[i]
  bbox_panel[4] <- bbox_panel[2] + (bbox_panel[3] - bbox_panel[1]) * factor_shape

  # Cropping the shapefiles (MUCH more efficient than calling bbox in tm_shape)
  countries_eq_panel <- sf::st_crop(countries_eq, bbox_panel)
  allreefs_panel <- sf::st_crop(allreefs, bbox_panel)

  # First plot the country borders
  m.p <- tmap::tm_shape(countries_eq_panel) +
    tmap::tm_polygons(
      col = "gray",
      border.col = "darkgray",
      lwd = 0.2
    ) +

    # Then the reef polygon
    tmap::tm_shape(allreefs_panel) +
    tmap::tm_fill(
      col = "top_threat",
      palette = "Set2"
    ) +
    tmap::tm_legend(show = F)

  # Save the panel map
  tmap::tmap_save(m.p, filename = here::here(glue::glue("plots/topthreat_panel_{v.bbox.panel$name[i]}.pdf")), width = ifelse(factor_shape == 1, full_width / 4, full_width / 2), height = full_width / 4, units = "cm", dpi = 600)
}
rm(allreefs_panel, countries_eq_panel, m.p)

# Create a reef_for_legend layer, of small size, to speed up plotting of the legend
reefs_for_legend <- dplyr::filter(allreefs, BCU_name == "Tanzania/Kenya") # Just a random BCU

# Define the pressure names so they appear nicely in the legend
reefs_for_legend$top_threat_name <- threat_names[reefs_for_legend$top_threat]
reefs_for_legend$top_threat_name <- factor(reefs_for_legend$top_threat_name, levels = threat_names[1:6])

# Plot the legend
top_threats_legend <-
  tmap::tm_shape(reefs_for_legend) +
  tmap::tm_fill(
    col = "top_threat_name",
    palette = "Set2",
    title = "Pressure",
    legend.is.portrait = F
  ) +
  tmap::tm_layout(
    legend.only = T,
    legend.position = c("center", "center"),
    title.position = c("center", "center")
  )

# Save the legend
tmap::tmap_save(top_threats_legend, filename = here::here(glue::glue("plots/topthreat_legend.pdf")), width = full_width, height = 2, units = "cm") # , dpi = 600)
rm(reefs_for_legend)

# Construct plot using magick
plot_global <- magick::image_read_pdf(here::here("plots/topthreat_global.pdf"), density = 900)

# Set up list to store the geographical panels
panel_list <- vector("list", length = length(panels))
names(panel_list) <- panels

for (i in names(panel_list)) {
  # Read panels in, in order intended, and save them in the list
  panel_list[[i]] <- magick::image_read_pdf(here::here(glue::glue("plots/topthreat_panel_{i}.pdf")), density = 900)

  # Add name of the region in the panel map
  panel_list[[i]] <- magick::image_annotate(panel_list[[i]], i,
    gravity = "North", size = 9, location = c("+0+30")
  )
}

# Read legend
legend <- magick::image_read_pdf(here::here("plots/topthreat_legend.pdf"), density = 900)

# Create a blank image
# Height is 2 * panels + global panel + legend
# Width is global panel
panel_height <- magick::image_info(panel_list[[1]])[["height"]]
global_height <- magick::image_info(plot_global)[["height"]]
global_width <- magick::image_info(plot_global)[["width"]]
legend_height <- magick::image_info(legend)[["height"]]
legend_width <- magick::image_info(legend)[["width"]]

plot_width <- global_width
plot_height <- global_height + 2 * panel_height + legend_height

plot_image <- magick::image_blank(width = plot_width, height = plot_height, color = "white")

# Add the first 3 panels
# Start with 0 width offset
width_offset <- 0
for (i in 1:3) {
  plot_image <- magick::image_composite(plot_image, panel_list[[i]], offset = glue::glue("+{width_offset}+0"))
  # Then increment the offset by the width of the panel, so the next panel can use it
  width_offset <- width_offset + magick::image_info(panel_list[[i]])[["width"]]
}

# Add the global plot
plot_image <- magick::image_composite(plot_image, plot_global, offset = glue::glue("+0+{image_info(panel_list[[i]])[['height']]}"))

# Add the last 3 panels
# Start with 0 width offset
width_offset <- 0
for (i in 4:6) {
  height_offset <- magick::image_info(panel_list[[i]])[["height"]] + image_info(plot_global)[["height"]]
  plot_image <- magick::image_composite(plot_image, panel_list[[i]], offset = glue::glue("+{width_offset}+{height_offset}"))
  # Then increment the offset by the width of the panel, so the next panel can use it
  width_offset <- width_offset + magick::image_info(panel_list[[i]])[["width"]]
}

# Add the legend
legend_height_offset <- plot_height - legend_height
legend_width_offset <- plot_width / 2 - legend_width / 2
plot_image <- magick::image_composite(plot_image, legend, offset = glue::glue("+{legend_width_offset}+{legend_height_offset}"))

# Plot final figure
image_write(plot_image, here("Figure 1.png"))

# Remove working files (empty "plots" folder)
file.remove(paste0("plots/", dir(here("plots"))))


#############################################################################################
# End FIGURE 1
#############################################################################################


#############################################################################################
#  FIGURE 2 (CUMULATIVE SCORE) AND SUPPLEMENTARY FIGURES 2 to 7 (INDIVIDUAL PRESSURES)
#############################################################################################

fs::dir_create(here::here("plots")) # Create plots folder if it doesn't exist

# Loop on the six pressures + cumulative score
for (i.threat in 1:7) {
  indicator <- vthreats[i.threat]
  indicator_title <- threat_names[i.threat]
  cat("Plotting", indicator_title, "\n")

  # Define breaks, labels and legend style for each pressure
  if (indicator == "num_ports") {
    indicator <- "num_ports_raw"
    indicator_breaks <- c(0:9)
    legend_labels <- as.character(0:8)
    legend_style <- "fixed"
  } else {
    indicator_breaks <- breaks[[indicator]]
    breaks_midpoint <- ceiling(length(indicator_breaks) / 2)
    breaks_midpoint_value <- indicator_breaks[[breaks_midpoint]]
    blank_breaks <- rep("", breaks_midpoint - 2)
    legend_labels <- c(min(indicator_breaks), blank_breaks, breaks_midpoint_value, blank_breaks, max(indicator_breaks))
    legend_style <- "cont"
  }

  # Plot global map
  m.global <- m +
    tmap::tm_shape(allreefs) +
    tmap::tm_fill(
      col = indicator,
      palette = RColorBrewer::brewer.pal(length(indicator_breaks), "OrRd"),
      style = legend_style,
      breaks = indicator_breaks,
      title.col = indicator_title,
      labels = legend_labels,
      showNA = FALSE
    ) +
    tmap::tm_legend(show = FALSE)

  # Save global map
  tmap::tmap_save(m.global, filename = here::here(glue::glue("plots/{vthreats[i.threat]}_global.pdf")), width = full_width, height = 4, units = "cm") # , dpi = 600)

  # Plot the six regional panels
  for (i in 1:nrow(v.bbox.panel)) {
    cat("Plotting panel", i, "\n")

    # Define bbox of panel region: here is where horiz.extent and fact.shape come into play
    factor_shape <- v.bbox.panel$factor_shape[i]
    bbox_panel[1] <- v.bbox.panel$xmin[i]
    bbox_panel[2] <- v.bbox.panel$ymin[i]
    bbox_panel[3] <- bbox_panel[1] + horiz.extent[i]
    bbox_panel[4] <- bbox_panel[2] + (bbox_panel[3] - bbox_panel[1]) * factor_shape

    # Cropping the shapefiles (MUCH more efficient than calling bbox in tm_shape)
    countries_eq_panel <- sf::st_crop(countries_eq, bbox_panel)
    allreefs_panel <- sf::st_crop(allreefs, bbox_panel)

    # First plot the country borders
    m.p <- tmap::tm_shape(countries_eq_panel) +
      tmap::tm_polygons(
        col = "gray",
        border.col = "darkgray",
        lwd = 0.2
      ) +

      # Then the reef polygons
      tmap::tm_shape(allreefs_panel) +
      tmap::tm_fill(
        col = indicator,
        palette = brewer.pal(length(indicator_breaks), "OrRd"),
        style = legend_style,
        breaks = indicator_breaks
      ) +
      tmap::tm_legend(show = F)

    # Save the panel map
    tmap::tmap_save(m.p, filename = here::here(glue::glue("plots/{vthreats[i.threat]}_panel_{v.bbox.panel$name[i]}.pdf")), width = ifelse(factor_shape == 1, full_width / 4, full_width / 2), height = full_width / 4, units = "cm", dpi = 600)
  }

  # Create a reef_for_legend layer, of small size, to speed up plotting of the legend
  reefs_for_legend <- allreefs[c(1000:1010), ] # Reduced dataset to speed up the plotting of the legend

  # Plot the legend
  # if (i.threat == 3) reefs_for_legend$num_ports[1] <- 1 # (percentile scale: max)
  leg <- tmap::tm_shape(reefs_for_legend) +
    tmap::tm_fill(
      size = 1,
      col = indicator,
      palette = brewer.pal(length(indicator_breaks), "OrRd"),
      style = legend_style,
      breaks = indicator_breaks,
      title = indicator_title,
      labels = legend_labels,
      showNA = FALSE,
      legend.is.portrait = F
    ) +
    tmap::tm_layout(legend.only = T, legend.position = c("center", "center"))

  # Save the legend
  tmap::tmap_save(leg, filename = here::here(glue::glue("plots/{vthreats[i.threat]}_legend.pdf")), width = 4.3, height = 2, units = "cm") # , dpi = 600)


  # Construct plot using magick
  plot_global <- magick::image_read_pdf(here::here(glue::glue("plots/{vthreats[i.threat]}_global.pdf")), density = 900)

  # Set up list to store the geographical panels
  panel_list <- vector("list", length = length(panels))
  names(panel_list) <- panels

  for (i in names(panel_list)) {
    # Read panels in, in order intended, and save them in the list
    panel_list[[i]] <- magick::image_read_pdf(here::here(glue::glue("plots/{vthreats[i.threat]}_panel_{i}.pdf")), density = 900)

    # Add name of the region in the panel map
    panel_list[[i]] <- magick::image_annotate(panel_list[[i]], i,
      gravity = "North", size = 9, location = c("+0+30")
    )
  }

  legend <- magick::image_read_pdf(here::here(glue::glue("plots/{vthreats[i.threat]}_legend.pdf")), density = 900)

  # Create a blank image
  panel_height <- magick::image_info(panel_list[[1]])[["height"]]
  global_height <- magick::image_info(plot_global)[["height"]]
  global_width <- magick::image_info(plot_global)[["width"]]
  legend_height <- magick::image_info(legend)[["height"]]
  legend_width <- magick::image_info(legend)[["width"]]
  plot_width <- global_width
  plot_height <- global_height + 2 * panel_height + legend_height
  plot_image <- magick::image_blank(width = plot_width, height = plot_height, color = "white")

  # Add the first 3 panels
  width_offset <- 0
  for (i in 1:3) {
    plot_image <- magick::image_composite(plot_image, panel_list[[i]], offset = glue::glue("+{width_offset}+0"))
    width_offset <- width_offset + magick::image_info(panel_list[[i]])[["width"]]
  }

  # Add the global plot
  plot_image <- magick::image_composite(plot_image, plot_global, offset = glue::glue("+0+{image_info(panel_list[[i]])[['height']]}"))

  # Add the last 3 panels
  width_offset <- 0
  for (i in 4:6) {
    height_offset <- magick::image_info(panel_list[[i]])[["height"]] + image_info(plot_global)[["height"]]
    plot_image <- magick::image_composite(plot_image, panel_list[[i]], offset = glue::glue("+{width_offset}+{height_offset}"))
    width_offset <- width_offset + magick::image_info(panel_list[[i]])[["width"]]
  }

  # Add the legend
  legend_height_offset <- plot_height - legend_height
  legend_width_offset <- plot_width / 2 - legend_width / 2
  plot_image <- magick::image_composite(plot_image, legend, offset = glue::glue("+{legend_width_offset}+{legend_height_offset}"))

  # Plot final figure
  
  # threat order: fishing, coast_pop, ports, tourism, sediment nutrients
  # Supplementary figure order: sediments, nutrients, fishing, coast_pop, tourism, ports
  SFig_threat_corresp <- c(4,5,7,6,2,3) # gives the supplementary figure number of each threat
  if (i.threat < 7) {
    file_name <- paste0("Figure S", SFig_threat_corresp[i.threat], ".png")
  } else {
    file_name <- "Figure 2.png"
  }
  magick::image_write(plot_image, file_name)

  # Remove working files (empty "plots" folder)
  file.remove(paste0("plots/", dir(here::here("plots"))))
}
