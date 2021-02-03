# Plot Figure 1, Figure 2 and Supplementary maps
# Marco Andrello
# 03/02/2021

rm(list=ls())

library(tidyverse)
library(sf)
library(here)
library(tmap)
library(RColorBrewer)
library(magick)

load(here("scripts","DataForAnalysis.RData"))

threat_names <- c("Fishing","Coastal dev","Industrial dev",
                  "Tourism","Sediments","Nutrients", "Cumulative impact score" )
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
for (i in 1 : 7) breaks[[i]] <- seq(0,1,0.25)
names(breaks) <- c(vthreats,"score.l")

# Plot countries at global scale
m <- tm_shape(countries_eq) +
  tm_polygons(col="gray",
              border.col="darkgray",
              lwd=0.2)

# Width of the figure
full_width <- 18.25

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
  -1e6, -3.5e6, NA, NA, "Australia and Melanesia"
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


#############################################################################################
#  FIGURE 1: TOP THREAT
#############################################################################################
allreefs_withBCU_prc$top.threat <- factor(allreefs_withBCU_prc$top.threat)

# Top threat, global map
m_global_topthreats <-  m +
  tm_shape(allreefs_withBCU_prc) +
  tm_fill(col = "top.threat",
          palette = "Set2") + 
  tmap::tm_legend(show = FALSE)
full_width <- 18.25
tmap_save(m_global_topthreats, filename = here::here(glue::glue("plots/topthreat_global_Set2.pdf")), width = full_width, height = 4, units = "cm")


# Plot the six regional panels
for (i in 1 : nrow(v.bbox.panel)) {
  cat("Plotting panel",i,"\n")
  factor_shape <- v.bbox.panel$factor_shape[i]
  # Define bbox of panel region: here is where horiz.extent and fact.shape come into play
  bbox_panel[1] <- v.bbox.panel$xmin[i]
  bbox_panel[2] <- v.bbox.panel$ymin[i]
  bbox_panel[3] <- bbox_panel[1] + horiz.extent[i]
  bbox_panel[4] <- bbox_panel[2] + (bbox_panel[3] - bbox_panel[1])*factor_shape
  # Cropping the shapefiles (MUCH more efficient than calling bbox in tm_shape)
  countries_eq_panel <- st_crop(countries_eq, bbox_panel)
  allreefs_withBCU_prc_panel <- st_crop(allreefs_withBCU_prc, bbox_panel)
  # First plot the country borders
  m.p <- tm_shape(countries_eq_panel) +
    tm_polygons(col="gray",
                border.col="darkgray",
                lwd=0.2) +
    # Then the reef pixels
    tm_shape(allreefs_withBCU_prc_panel) +
    tm_fill(col = "top.threat",
            palette = "Set2") +
    tmap::tm_legend(show = F)
  tmap_save(m.p, filename = here::here(glue::glue("plots/topthreat_panel_{v.bbox.panel$name[i]}.pdf")), width = ifelse(factor_shape == 1, full_width/4, full_width/2), height = full_width/4, units = "cm", dpi = 600)
}

# Plot the legend
reefs_for_legend <- filter(allreefs_withBCU_prc, ReefName == "Tanzania/Kenya") # Just a random BCU
reefs_for_legend$top.threat.name <- threat_names[reefs_for_legend$top.threat]
reefs_for_legend$top.threat.name <- factor(reefs_for_legend$top.threat.name, levels=threat_names)
top_threats_legend <-
  tm_shape(reefs_for_legend) +
  tm_fill(col = "top.threat.name",
          palette = "Set2",
          title = "Threat",
          legend.is.portrait = F) + 
  tm_layout(legend.only = T,
            legend.position=c("center","center"),
            title.position=c("center","center"))
top_threats_legend

tmap_save(top_threats_legend, filename = here::here(glue::glue("plots/topthreat_legend.pdf")), width = full_width, height = 2, units = "cm")#, dpi = 600)


# Construct plot using magick
plot_global <- image_read_pdf(here::here("plots/topthreat_global.pdf"),density=600)

# Read panels in, in order intended, and save them to a list


panel_list <- vector("list", length = length(panels))
names(panel_list) <- panels

for(i in names(panel_list)) {
  panel_list[[i]] <- image_read_pdf(here::here(glue::glue("plots/topthreat_panel_{i}.pdf")),density=600)
  panel_list[[i]] <- image_annotate(panel_list[[i]],i,
                                    gravity="North", size=9, location=c("+0+30"))
}
# panel_list[[1]]

legend <- image_read_pdf(here::here("plots/topthreat_legend.pdf"),density=600)

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

image_write(plot_image, here::here("plots/final_plot_topthreat.png"))


#############################################################################################
# End FIGURE 1
#############################################################################################


#############################################################################################
#  FIGURE 2 (CUMULATIVE SCORE) AND SUPPLEMENTARY FIGURES (INDIVIDUAL THREAT)
#############################################################################################
for (i.threat in 1 : 7) {
  indicator <- vthreats[i.threat]
  indicator_title <- threat_names[i.threat]
  cat("Plotting",indicator_title,"\n")
  
  if(indicator == "num_ports") {
    indicator <- "num_ports_raw"
    indicator_breaks <- c(0:9)
    legend_labels <- as.character(0:8)
    legend_style <- "fixed"
  } else {
    indicator_breaks <- breaks[[indicator]]
    breaks_midpoint <- ceiling(length(indicator_breaks)/2)
    breaks_midpoint_value <- indicator_breaks[[breaks_midpoint]]
    blank_breaks <- rep("", breaks_midpoint - 2)
    legend_labels <- c(min(indicator_breaks), blank_breaks, breaks_midpoint_value, blank_breaks, max(indicator_breaks))
    legend_style <- "cont"
  }
  # global map 
  m.global <- m +
    tmap::tm_shape(allreefs_withBCU_prc) +
    tm_fill(col = indicator,
            palette = brewer.pal(length(indicator_breaks), "OrRd"),
            style = legend_style,
            breaks = indicator_breaks,
            title.col = indicator_title,
            labels = legend_labels,
            showNA = FALSE) +
    tmap::tm_legend(show = FALSE)
  tmap_save(m.global, filename = here::here(glue::glue("plots/individual/{vthreats[i.threat]}_global.pdf")), width = full_width, height = 4, units = "cm") #, dpi = 600)
  
  
  # Plot the six regional panels
  for (i in 1 : nrow(v.bbox.panel)) {
    cat("Plotting panel",i,"\n")
    factor_shape <- v.bbox.panel$factor_shape[i]
    
    # Define bbox of panel region: here is where horiz.extent and fact.shape come into play
    bbox_panel[1] <- v.bbox.panel$xmin[i]
    bbox_panel[2] <- v.bbox.panel$ymin[i]
    bbox_panel[3] <- bbox_panel[1] + horiz.extent[i]
    bbox_panel[4] <- bbox_panel[2] + (bbox_panel[3] - bbox_panel[1])*factor_shape
    
    # Cropping the shapefiles (MUCH more efficient than calling bbox in tm_shape)
    countries_eq_panel <- st_crop(countries_eq, bbox_panel)
    allreefs_withBCU_prc_panel <- st_crop(allreefs_withBCU_prc, bbox_panel)
    
    # First plot the country borders
    m.p <- tm_shape(countries_eq_panel) +
      tm_polygons(col="gray",
                  border.col="darkgray",
                  lwd=0.2) +
      # Then the reef pixels
      tm_shape(allreefs_withBCU_prc_panel) +
      tm_fill(col = indicator,
              palette = brewer.pal(length(indicator_breaks), "OrRd"),
              style = legend_style,
              breaks = indicator_breaks) +
      tmap::tm_legend(show = F)
    tmap_save(m.p, filename = here::here(glue::glue("plots/individual/{vthreats[i.threat]}_panel_{v.bbox.panel$name[i]}.pdf")), width = ifelse(factor_shape == 1, full_width/4, full_width/2), height = full_width/4, units = "cm", dpi = 600)
  }
  
  # Plot the legend
  reefs_for_legend <- allreefs_withBCU_prc[c(1000:1010),] # Reduced dataset to speed up the plotting of the legend
  # if (i.threat == 3) reefs_for_legend$num_ports[1] <- 1 # (percentile scale: max)
  leg <- tmap::tm_shape(reefs_for_legend) +
    tm_fill(size = 1,
            col = indicator,
            palette = brewer.pal(length(indicator_breaks), "OrRd"),
            style = legend_style,
            breaks = indicator_breaks,
            title = indicator_title,
            labels = legend_labels,
            showNA = FALSE,
            legend.is.portrait = F) +
    tmap::tm_layout(legend.only = T, legend.position = c("center","center"))
  leg
  tmap_save(leg, filename = here::here(glue::glue("plots/individual/{vthreats[i.threat]}_legend.pdf")), width = 4.3, height = 2, units = "cm")#, dpi = 600)
  
  
  # Construct plot using magick
  plot_global <- image_read_pdf(here::here(glue::glue("plots/individual/{vthreats[i.threat]}_global.pdf")),density=600)
  
  # Read panels in, in order intended, and save them to a list
  panel_list <- vector("list", length = length(panels))
  names(panel_list) <- panels
  for(i in names(panel_list)) {
    panel_list[[i]] <- image_read_pdf(here::here(glue::glue("plots/individual/{vthreats[i.threat]}_panel_{i}.pdf")),density=600)
    panel_list[[i]] <- image_annotate(panel_list[[i]],i,
                                      gravity="North", size=9, location=c("+0+30"))
  }
  # panel_list[[1]]
  legend <- image_read_pdf(here::here(glue::glue("plots/individual/{vthreats[i.threat]}_legend.pdf")),density=600)
  # Create a blank image
  panel_height <- image_info(panel_list[[1]])[["height"]]
  global_height <- image_info(plot_global)[["height"]]
  global_width <- image_info(plot_global)[["width"]]
  legend_height <- image_info(legend)[["height"]]
  legend_width <- image_info(legend)[["width"]]
  plot_width <- global_width
  plot_height <- global_height + 2 * panel_height + legend_height
  plot_image <- image_blank(width = plot_width, height = plot_height, color = "white")
  # Add the first 3 panels
  width_offset <- 0
  for (i in 1:3) {
    plot_image <- image_composite(plot_image, panel_list[[i]], offset = glue::glue("+{width_offset}+0"))
    width_offset <- width_offset + image_info(panel_list[[i]])[["width"]]
  }
  # Add the global plot
  plot_image <- image_composite(plot_image, plot_global, offset = glue::glue("+0+{image_info(panel_list[[i]])[['height']]}"))
  # Add the last 3 panels
  width_offset <- 0
  for (i in 4:6) {
    height_offset <- image_info(panel_list[[i]])[["height"]] + image_info(plot_global)[["height"]]
    plot_image <- image_composite(plot_image, panel_list[[i]], offset = glue::glue("+{width_offset}+{height_offset}"))
    width_offset <- width_offset + image_info(panel_list[[i]])[["width"]]
  }
  legend_height_offset <- plot_height - legend_height
  legend_width_offset <- plot_width / 2 - legend_width / 2
  plot_image <- image_composite(plot_image, legend, offset = glue::glue("+{legend_width_offset}+{legend_height_offset}"))
  image_write(plot_image, here::here(glue::glue("plots/final_plot_{vthreats[i.threat]}.png")))
}






