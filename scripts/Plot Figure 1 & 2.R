# Plot Figure 1 and Figure 2
# Global map of threats, either synthetic score or top threat
# Marco Andrello
# 24/06/2020

rm(list=ls())

library(tidyverse)
library(sf)
library(here)
library(tmap)
library(RColorBrewer)
library(ggplot2)
library(scales)

load(here("scripts","DataForAnalysis.RData"))

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



#############################################################################################
#  FIGURE 1: TOP THREAT
#############################################################################################
# Set up legend breaks, labels and style
indicator_breaks <- seq(0,1,0.25)
breaks_midpoint <- ceiling(length(indicator_breaks)/2)
breaks_midpoint_value <- indicator_breaks[[breaks_midpoint]]
blank_breaks <- rep("", breaks_midpoint - 2)
legend_labels <- c(min(indicator_breaks), blank_breaks, breaks_midpoint_value, blank_breaks, max(indicator_breaks))
legend_style <- "cont"

# Num_ports_raw should be a factor to have good legend colors
allreefs_withBCU_prc_centroids$num_ports_raw <- factor(allreefs_withBCU_prc_centroids$num_ports_raw,levels=c(0:8))

# Set up colors: 6 colors with 10 shadings
base.c <- cbind("white",brewer.pal(6,"Set1"))
base.c <- base.c[c(1,5,6,4,2,3),] # red (gravity), orange (pop_count), yellow(ports), purple (tourism), blue (sediment), green (nutrient)
# Set up palettes for the 6 threats
colors <- matrix(NA,nrow=10,ncol=6)
for (i in 1 : length(vthreats)) {
  if(i == 3) next
  ramp <- colour_ramp(base.c[i,])
  colors[,i] <- ramp(seq(0, 1, length = 10))
}
colnames(colors) <- vthreats
ramp <- colour_ramp(base.c[3,])
colors_ports <- ramp(seq(0, 1, length=9))

# Draw legends
reefs_for_legend <- filter(allreefs_withBCU_prc_centroids, ReefName == "Tanzania/Kenya") # Just a random BCU
top_threats_legend <-
  tm_shape(reefs_for_legend, filter=(reefs_for_legend$top.threat == 1)) +
  tm_squares(size = 0.25,
             col = "grav_NC",
             palette = colors[,"grav_NC"],
             style = legend_style,
             breaks = indicator_breaks,
             title.col = "Fishing",
             labels = legend_labels,
             legend.col.is.portrait = F) +
  tm_shape(reefs_for_legend, filter=(reefs_for_legend$top.threat == 2)) +
  tm_squares(size = 0.25,
             col = "pop_count",
             palette = colors[,"pop_count"],
             style = legend_style,
             breaks = indicator_breaks,
             title.col = "Coastal \ndevelopment",
             labels = legend_labels,
             legend.col.is.portrait = F) +
  tm_shape(reefs_for_legend, filter=(reefs_for_legend$top.threat == 3)) +
  tm_squares(size = 0.25,
             col = "num_ports_raw",
             palette = colors_ports,
             style = "fixed",
             title.col = "Industrial \ndevelopment",
             legend.col.is.portrait = F) +
  tm_shape(reefs_for_legend, filter=(reefs_for_legend$top.threat == 4)) +
  tm_squares(size = 0.25,
             col = "reef_value",
             palette = colors[,"reef_value"],
             style = legend_style,
             breaks = indicator_breaks,
             title.col = "Tourism",
             labels = legend_labels,
             legend.col.is.portrait = F) +
  tm_shape(reefs_for_legend, filter=(reefs_for_legend$top.threat == 5)) +
  tm_squares(size = 0.25,
             col = "sediment",
             palette = colors[,"sediment"],
             style = legend_style,
             breaks = indicator_breaks,
             title.col = "Sediments",
             labels = legend_labels,
             legend.col.is.portrait = F) +
  tm_shape(reefs_for_legend, filter=(reefs_for_legend$top.threat == 6)) +
  tm_squares(size = 0.25,
             col = "nutrient",
             palette = colors[,"nutrient"],
             style = legend_style,
             breaks = indicator_breaks,
             title.col = "Nutrients",
             labels = legend_labels,
             legend.col.is.portrait = F) +
tm_layout(legend.only = T, legend.stack="horizontal",
          legend.position=c("center","center"),
          title.position=c("center","center"))

png(paste0("top_threats_legend.png"), width = 18.5, height = 4.3, units = "cm", res = 600)
print(top_threats_legend)
dev.off()

# Top threat, global map
m_global_topthreats <- m +
  tm_shape(allreefs_withBCU_prc_centroids, filter=(allreefs_withBCU_prc_centroids$top.threat == 1)) +
  tm_squares(size = 1,
             scale = 0.01,
             col = "grav_NC",
             palette = colors[,"grav_NC"],
             border.lwd = NA,
             style = legend_style,
             breaks = indicator_breaks) +
  tm_shape(allreefs_withBCU_prc_centroids, filter=(allreefs_withBCU_prc_centroids$top.threat == 2)) +
  tm_squares(size = 1,
             scale = 0.01,
             col = "pop_count",
             palette = colors[,"pop_count"],
             border.lwd = NA,
             style = legend_style,
             breaks = indicator_breaks) +
  tm_shape(allreefs_withBCU_prc_centroids, filter=(allreefs_withBCU_prc_centroids$top.threat == 3)) +
  tm_squares(size = 1,
             scale = 0.01,
             col = "num_ports_raw",
             palette = colors_ports,
             border.lwd = NA,
             style = "fixed") +
  tm_shape(allreefs_withBCU_prc_centroids, filter=(allreefs_withBCU_prc_centroids$top.threat == 4)) +
  tm_squares(size = 1,
             scale = 0.01,
             col = "reef_value",
             palette = colors[,"reef_value"],
             border.lwd = NA,
             style = legend_style,
             breaks = indicator_breaks) +
  tm_shape(allreefs_withBCU_prc_centroids, filter=(allreefs_withBCU_prc_centroids$top.threat == 5)) +
  tm_squares(size = 1,
             scale = 0.01,
             col = "sediment",
             palette = colors[,"sediment"],
             border.lwd = NA,
             style = legend_style,
             breaks = indicator_breaks) +
  tm_shape(allreefs_withBCU_prc_centroids, filter=(allreefs_withBCU_prc_centroids$top.threat == 6)) +
  tm_squares(size = 1,
             scale = 0.01,
             col = "nutrient",
             palette = colors[,"nutrient"],
             border.lwd = NA,
             style = legend_style,
             breaks = indicator_breaks) +
  tmap::tm_legend(show = FALSE)

png("top_threats_global.png", width = 18.5, height = 5, units = "cm", res = 900)
print(m_global_topthreats)
dev.off()


# Prepare panels of top threats
panel.list <- grid::gList()
for (i in 1 : nrow(v.bbox.panel)) {
  # Define bbox of panel region
  bbox_panel[1] <- v.bbox.panel$xmin[i]
  bbox_panel[2] <- v.bbox.panel$ymin[i]
  bbox_panel[3] <- bbox_panel[1] + horiz.extent[i]
  bbox_panel[4] <- bbox_panel[2] + (bbox_panel[3] - bbox_panel[1])*fact.shape[i] # So it has a square shape or rectangular (two-squares sidebyside)

  # Plot regional panel
  m.p <- tm_shape(countries_eq, bbox = bbox_panel) +
    tm_polygons(col="gray",
                border.col="darkgray",
                lwd=0.2) +
    tm_shape(allreefs_withBCU_prc_centroids, filter=(allreefs_withBCU_prc_centroids$top.threat == 1), bbox = bbox_panel) +
    tm_squares(size = 1,
               scale = 0.025*fact.shape[i],
               shape = 15,
               col = "grav_NC",
               palette = colors[,"grav_NC"],
               border.lwd = NA,
               style = legend_style,
               breaks = indicator_breaks) +
    tm_shape(allreefs_withBCU_prc_centroids, filter=(allreefs_withBCU_prc_centroids$top.threat == 2), bbox = bbox_panel) +
    tm_squares(size = 1,
               scale = 0.025*fact.shape[i],
               shape = 15,
               col = "pop_count",
               palette = colors[,"pop_count"],
               border.lwd = NA,
               style = legend_style,
               breaks = indicator_breaks) +
    tm_shape(allreefs_withBCU_prc_centroids, filter=(allreefs_withBCU_prc_centroids$top.threat == 3), bbox = bbox_panel) +
    tm_squares(size = 1,
               scale = 0.025*fact.shape[i],
               shape = 15,
               col = "num_ports_raw",
               palette = colors_ports,
               border.lwd = NA,
               style = "fixed") +
    tm_shape(allreefs_withBCU_prc_centroids, filter=(allreefs_withBCU_prc_centroids$top.threat == 4), bbox = bbox_panel) +
    tm_squares(size = 1,
               scale = 0.025*fact.shape[i],
               shape = 15,
               col = "reef_value",
               palette = colors[,"reef_value"],
               border.lwd = NA,
               style = legend_style,
               breaks = indicator_breaks) +
    tm_shape(allreefs_withBCU_prc_centroids, filter=(allreefs_withBCU_prc_centroids$top.threat == 5), bbox = bbox_panel) +
    tm_squares(size = 1,
               scale = 0.025*fact.shape[i],
               shape = 15,
               col = "sediment",
               palette = colors[,"sediment"],
               border.lwd = NA,
               style = legend_style,
               breaks = indicator_breaks) +
    tm_shape(allreefs_withBCU_prc_centroids, filter=(allreefs_withBCU_prc_centroids$top.threat == 6), bbox = bbox_panel) +
    tm_squares(size = 1,
               scale = 0.025*fact.shape[i],
               shape = 15,
               col = "nutrient",
               palette = colors[,"nutrient"],
               border.lwd = NA,
               style = legend_style,
               breaks = indicator_breaks) +
    tmap::tm_legend(show = FALSE)
  panel.list[[i]] <- m.p
}

# Print regional maps of top threats on files
for (i in 1 : 6) {
  cat("plotting",i,"\n")
  png(paste0("top_threats_panel",i,".png"), width = 4.3, height = 4.3, units = "cm", res = 600)
  print(panel.list[[i]])
  dev.off()
}


##
# End FIGURE 1
#############################################################################################



#############################################################################################
#  FIGURE 2: CUMULATIVE SCORE
#############################################################################################

indicator <- "score.l"
indicator_title <- "Cumulative impact score"

# Set up scale, colors and legend elements
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

# Cumulative impact score, global map
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

png("cumulative_global.png",width = 18.5, height = 5, units = "cm", res = 600)
m.global.cumulative
dev.off()

# Plot regional panels
panel.list.cumulative <- grid::gList()
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
               palette = brewer.pal(length(indicator_breaks), "OrRd"),
               shape = 15,
               scale = 0.05*fact.shape[i],
               style = legend_style,
               breaks = indicator_breaks) +
    tmap::tm_legend(show = F)

  panel.list.cumulative[[i]] <- m.p
}
# Print regional maps of cumulative score on files
for (i in 1 : 6) {
  cat("plotting",i,"\n")
  png(paste0("cumulative_panel",i,".png"), width = 4.3, height = 4.3, units = "cm", res = 600)
  print(panel.list.cumulative[[i]])
  dev.off()
}

# Legend
reefs_for_legend <- allreefs_withBCU_prc_centroids[c(1000:1010),] # Reduced dataset to speed up the plotting of the legend
breaks_logscale <- seq(0,1,0.25)
png("cumulative_legend.png", width = 4.3, height = 4.3, units = "cm", res = 600)
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
print(leg)
dev.off()

# END FIGURE 2
#############################################################################################









