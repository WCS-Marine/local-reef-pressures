# Plot Figure S1 to S6
# Global map of individual threats
# Marco Andrello
# 23/09/2020

rm(list=ls())

library(tidyverse)
library(sf)
library(here)
library(tmap)
library(RColorBrewer)
library(ggplot2)
library(scales)

load(here("paper","DataForAnalysis.RData"))

# Plot countries
m <- tm_shape(countries_eq) +
  tm_polygons(col="gray",
              border.col="darkgray",
              lwd=0.2)


# Set up breaks
breaks <- list()
for (i in 1 : 7) breaks[[i]] <- seq(0,1,0.25)
names(breaks) <- c(vthreats,"score.l")


# Define margins of panel regions
bbox_panel <- st_bbox(countries_eq)
v.bbox.panel  <- matrix(c(-13e6, 1.1e6, -10e6, 3.5e6,   # Middle East and North Africa
                          -13e6, -3e6, -10e6, 0.5e6,    # East Africa
                          -9e6, -1e6, -5.5e6, 2.6e6,    # Indian Ocean
                          -6e6, -1.3e6, -1e6, 2.2e6,    # Coral Triangle
                          13e6, 2e6, 16e6, 3e6,         # Caribbean and Bahamas
                          -1e6, -3.5e6, 3.5e6, 0.3e6),  # Australia and Melanesia
                        byrow=T, ncol=4)
colnames(v.bbox.panel) <- names(bbox_panel)
v.bbox.panel <- as.data.frame(v.bbox.panel)
horiz.extent <- rep(3e6,nrow(v.bbox.panel))
horiz.extent[c(4,6)] <- c(6.8e6,5e6)
fact.shape <- rep(1,6)
fact.shape[c(4,5)] <- 0.5
# text.size.scalebar <- rep(0.25, 7); text.size.scalebar[5] <- 0.125

# Set up colors: 6 colors with 10 shadings
base.c <- cbind("white",brewer.pal(6,"Set1"))
base.c <- base.c[c(1,5,6,4,2,3),] # red (gravity), orange (pop_count), yellow(ports), purple (tourism), blue (sediment), green (nutrient)
# Set up palettes for the 6 threats
colors <- list()
for (i in 1 : length(vthreats)) {
  ramp <- colour_ramp(base.c[i,])
  colors[[i]] <- ramp(seq(0, 1, length = 10))
  if (i == 3) colors[[i]] <- ramp(seq(0, 1, length = 9))
}
names(colors) <- vthreats

##
#  SUPPLEMENTARY FIGURES: MAPS OF INDIVIDUAL THREATS
###


v.indicator_title <- c("Fishing", "Coastal \ndevelopment", "Industrial \ndevelopment", "Tourism", "Sediments", "Nutrients")
reefs_for_legend <- filter(allreefs_withBCU_prc_centroids,ReefName == "Tanzania/Kenya") # Reduced dataset to speed up the plotting of the legend
breaks <- seq(0,1,0.25)

for (i.indicator in 1 : 6) {
  indicator <- vthreats[i.indicator]
  indicator_title <- v.indicator_title[i.indicator]
  cat(indicator,"\n")

  # Set up scale, colors and legend elements
  if(indicator == "num_ports") {
    indicator_breaks <- seq(0,9,1)
    legend_labels <- as.character(seq(0,8,1))
    legend_style <- "fixed"
  } else {
    indicator_breaks <- breaks
    breaks_midpoint <- ceiling(length(indicator_breaks)/2)
    breaks_midpoint_value <- indicator_breaks[[breaks_midpoint]]
    blank_breaks <- rep("", breaks_midpoint - 2)
    legend_labels <- c(min(indicator_breaks), blank_breaks, breaks_midpoint_value, blank_breaks, max(indicator_breaks))
    legend_style <- "cont"
  }

  # Plot global map
  m.global <- m +
    tmap::tm_shape(allreefs_withBCU_prc_centroids) +
    tm_squares(size = 1,
               col = indicator,
               palette = colors[[i.indicator]],
               shape = 15,
               scale = 0.01,
               style = legend_style,
               breaks = indicator_breaks,
               showNA = TRUE) +
    tmap::tm_legend(show = FALSE)
  png(paste0(indicator,"_global.png"),width = 18.5, height = 5, units = "cm", res = 900)
  cat("plotting global\n")
  print(m.global)
  dev.off()


    # Plot regional panels
    for (i in 1 : nrow(v.bbox.panel)) {
      # Define bbox of panel region
      bbox_panel[1] <- v.bbox.panel$xmin[i]
      bbox_panel[2] <- v.bbox.panel$ymin[i]
      bbox_panel[3] <- bbox_panel[1] + horiz.extent[i]
      bbox_panel[4] <- bbox_panel[2] + (bbox_panel[3] - bbox_panel[1])*fact.shape[i] # So it has a square shape or rectangular (two-squares sidebyside)
      m.p <- tm_shape(countries_eq, bbox = bbox_panel) +
        tm_polygons(col="gray",
                    border.col="darkgray",
                    lwd=0.2) +
        tm_shape(allreefs_withBCU_prc_centroids, bbox = bbox_panel) +
        tm_squares(size = 1,
                   col = indicator,
                   palette = colors[[i.indicator]],
                   shape = 15,
                   scale = 0.05*fact.shape[i],
                   style = legend_style,
                   breaks = indicator_breaks,
                   showNA = TRUE) +
        tmap::tm_legend(show = FALSE)
      cat("plotting panel",i,"\n")
      png(paste0(indicator,"_panel",i,".png"), width = 4.3, height = 4.3, units = "cm", res = 600)
      print(m.p)
      dev.off()
    }

  # Print legend
  png(paste0(indicator,"_legend.png"), width = 4.3, height = 4.3, units = "cm", res = 600)
  leg <- tmap::tm_shape(reefs_for_legend) +
    tm_squares(size = 1,
               col = indicator,
               palette = colors[[i.indicator]],
               style = legend_style,
               breaks = indicator_breaks,
               title.col = indicator_title,
               labels = legend_labels,
               showNA = FALSE,
               legend.col.is.portrait=F) +
    tmap::tm_layout(legend.only = T, legend.position = c("center","center"))
  print(leg)
  dev.off()

}
