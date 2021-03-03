# Plot one BCU
rm(list=ls())

library(sf)
library(here)
library(tmap)
library(RColorBrewer)
library(ggplot2)
library(scales)

load(here("scripts","DataForAnalysis.RData"))
load(here("scripts","PolygonsDataset.RData"))


name_bcu <- "Northern Papua"
allreefs_withBCU_prc %>% filter(ReefName == name_bcu) %>% st_sf() -> bcu.focal
bcu.focal_means <- apply(as.data.frame(bcu.focal)[,c("grav_NC","pop_count","reef_value","sediment","nutrient")],
                         2,
                         mean)
bcu.focal_medians <- apply(as.data.frame(bcu.focal)[,c("grav_NC","pop_count","reef_value","sediment","nutrient")],
                           2,
                           median)

text.annotation <- paste0("mean = ",
      format(bcu.focal_means,trim=T,digits=2),
      "\nmedian = ",
      format(bcu.focal_medians,trim=T,digits=2))

countries.focal <- st_crop(countries_eq, st_bbox(bcu.focal))

png("Figure 5.png",width=25,height=15,units="cm",res=400)
tm_shape(countries.focal) +
  tm_polygons(col="gray",
              border.col="darkgray",
              lwd=0.2) +
tm_shape(bcu.focal) +
  tm_polygons(col=c("grav_NC","pop_count","reef_value","sediment","nutrient"),
              breaks=seq(0,1,0.25)) +
  tm_credits(text.annotation, position=c("center", "bottom")) +
  tm_legend(legend.position=c("left", "center")) +
  tm_facets(nrow=3)
dev.off()


summary(bcu.focal$reef_value)

