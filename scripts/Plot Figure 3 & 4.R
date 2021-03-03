# Barplots to compare BCUs and boxplots to compare subregions of BCUs

rm(list=ls())

library(tidyverse)
library(here)
library(sf)
library(RColorBrewer)
library(grid)

load(here("scripts","DataForAnalysis.RData"))
load(here("data","allreefs.RData"))

# # Read region names
# bcus_regions <- readRDS(here::here("scripts", "bcus_regions.rds"))
# regions <- bcus_regions[[1]]
# for (i in 2 : 83) regions <- rbind(regions,bcus_regions[[i]])
# names(regions)[1] <- "BCU_name"

# GRAPHICAL PARAMETERS
# Set theme
theme_set(theme_minimal(10))
theme_update(axis.title = element_blank(),
             legend.text = element_text(size=7),
             legend.key.size = unit(.5, "cm"),
             legend.position = "bottom",
             plot.margin = margin(0.2,0.2,0.2,0.2,"cm"),
             axis.title.x = element_text(margin=margin(0.2,0,0,0,"cm")))

# Global medians
glob.median <- vector()
for (i in 1 : length(vthreats)) {
  indicator <- vthreats[i]
  glob.median[i] <- median(as.data.frame(allreefs)[,indicator],na.rm=T)
}
names(glob.median) <- vthreats
glob.median[7] <- median(allreefs$cumul_score)

# ## Check that the median percentile is the same as the percentile of median values
# data.bcu <- filter(allreefs_centroids, BCU_name == "Northern Papua")
# # Using grav_NC. Which pixel is the median for raw values
# id.median <- which(data.bcu$grav_NC_raw == median(data.bcu$grav_NC_raw))
# # What is its raw value
# data.bcu$grav_NC_raw[id.median]
# # What is its percentile
# data.bcu$grav_NC[id.median]
# ecdf(allreefs_centroids$grav_NC_raw)(data.bcu$grav_NC_raw[id.median]) # This is to double check.
# # What is the median percentle
# median(data.bcu$grav_NC)
# # OK, they are the same
# rm(data.bcu, id.median)


# Retain only BCUs
data <- allreefs
data <- data[data$is.bcu == "BCUs",]


# Median per BCU
data %>%
  select(grav_NC, pop_count, num_ports, reef_value, sediment, nutrient, cumul_score, BCU_name, Region) %>%
  group_by(BCU_name) %>%
  summarise(fishing = median(grav_NC,na.rm=T),
            coast_dev = median(pop_count,na.rm=T),
            industr_dev = median(num_ports,na.rm=T),
            tourism = median(reef_value,na.rm=T),
            sediments = median(sediment,na.rm=T),
            nutrients = median(nutrient,na.rm=T),
            cumulative = median(cumul_score,na.rm=T),
            Region = unique(Region)) ->
  ehe
ehe$Region <- factor(ehe$Region)

# Calculate top threat
v.threats.new <- c("fishing","coast_dev","industr_dev","tourism","sediments","nutrients")
# First threat
top_threat <- apply(as.data.frame(ehe)[,v.threats.new],1,which.max)
table(v.threats.new[top_threat]) / sum(table(v.threats.new[top_threat]))
ehe$top_threat <- top_threat
ehe$first.threat <- vthreats[top_threat]
# Second threat
second.threat <- rep(NA,83)
for(i in  1: 83) {
  ehe.i <- as.data.frame(ehe)[i,2:7]
  second.threat[i] <- names(which.max(ehe.i[-ehe$top_threat[i]]))
}
ehe$second.threat <- second.threat
top_threats_table <- as.data.frame(ehe)[,c(1:9,11:13)]
save(top_threats_table, file="TopThreat_RawValuePrc_BCUMedians_2021_02_10.RData")
write.csv(top_threats_table, file="TopThreat_RawValuePrc_BCUMedians_2021_02_10.csv")
v.threats.new[7] <- "cumulative"

# Chi-square : observed top threats (in BCUs) vs expected (in all reefs)
exp.tt <- table(allreefs$top_threat)
obs.tt <- table(factor(top_threats_table$top_threat,levels=c(1:6)))
chisq.test(rbind(exp.tt, obs.tt))
# chisq.test(obs.tt, p = exp.tt/sum(exp.tt))
# chisq.test(obs.tt, p = exp.tt, rescale.p = T)
# chisq.test(obs.tt, p = exp.tt, rescale.p = T, simulate.p.value = T)


# Arrange BCU by region
ehe.arranged <- ehe %>% arrange(Region) %>% mutate(x.order=c(1:83))

# Wrap region names
ehe.arranged$Region_wrap <- str_wrap(ehe.arranged$Region, width=11)

# Pivot threats
a <- ehe.arranged
st_geometry(a) <- NULL
a %>% select(BCU_name, fishing, coast_dev, industr_dev, tourism, sediments, nutrients, x.order, Region, Region_wrap) %>% 
  pivot_longer(c(fishing, coast_dev, industr_dev, tourism, sediments, nutrients), names_to="threat", values_to="percentile") -> a1
a1$threat <- factor(a1$threat, levels=unique(a1$threat))

theme_update(axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 7),
             strip.text.y = element_text(angle = 0, size=7))
png("Figure top_threat_BCU.png",width=10, height=20, units="cm", res=600)
p <- ggplot(a1,aes(y=BCU_name)) +
  geom_point(aes(x=percentile,colour=threat)) +
  scale_color_brewer(palette="Set2") +
  labs(y = "",
       x = "Percentile",
       color = "Threat") +
  facet_grid(rows = vars(Region_wrap), scales="free_y", space="free_y")
print(p)
dev.off()

# Barplot cumulative impact
a %>% select(BCU_name, cumulative, x.order, Region, Region_wrap) -> a2

theme_update(axis.text.y = element_blank(),
             strip.text.y = element_blank())
png("Figure cum_threat_BCU.png",width=10, height=20, units="cm", res=300)
ggplot(a2,aes(y=BCU_name)) +
  geom_col(aes(x=cumulative)) +
  geom_vline(aes(xintercept = glob.median[7]), linetype="dashed", size = 0.25, show.legend=F) +
  scale_x_reverse() +
 facet_grid(rows = vars(Region_wrap), scales="free_y", space="free_y")+
  labs(y = "",
       x = "Cumulative impact score")
dev.off()


# FIGURE 4. Boxplot of MEDIAN BCU percentiles by region
theme_update(plot.title = element_text(hjust = 0.5, size = 10),
             plot.margin = margin(0.2,0.2,0.2,0.2,"cm"),
             axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 7),
             axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 7))
title.text <- c("Fishing","Coastal\ndevelopment","Industrial\ndevelopment","Tourism","Sediments","Nutrients","Cumulative")
for (i in 1 : length(v.threats.new)) {
  if (i == 3) next
  indicator <- v.threats.new[i]
  png(paste0("Boxplot_",indicator,".png"), width=10, height=4, units="cm", res=300)
  a <- 
    ggplot(ehe, aes(y=reorder(Region,!!sym(indicator),FUN=median,na.rm=T),x=!!sym(indicator))) +
    geom_boxplot(fill=colors[i], size=0.1, outlier.size = 0.1, show.legend=F) +
    geom_vline(aes(xintercept = glob.median[i]), linetype="dashed", size = 0.25, show.legend=F) +
    scale_x_continuous(limits=c(0,1)) +
    labs(title=title.text[i], y="", x="")
  print(a)
  dev.off()

  cat(indicator, length(which(as.data.frame(ehe)[indicator] > glob.median[i])),"higher than global median\n")
}







# region.ymin <- rep(0.9, length(levels(ehe.arranged$Region)))
# region.ymax <- rep(1.1, length(levels(ehe.arranged$Region)))
# region.ymin[seq(1,9,2)] <- 0.9 - 0.1
# region.ymax[seq(1,9,2)] <- 1.1 - 0.1
# region.names <- levels(ehe.arranged$Region)
# region.names[7] <- "Middle East\nand North Africa"
# 
# # Vertical lines to separate regions
# sep.region <- cumsum(as.vector(table(ehe.arranged$Region))) + 0.5
# sep.region <- sep.region[1:8]
# 
# # Colours identify threats
# # (Same colours used in Figure 1 (map of top threat for all coral reef cells))
# col.threats <- brewer.pal(6,"Set2")
# colors <- c("Fishing" = col.threats[1],
#             "Coastal\ndevelopment" = col.threats[2],
#             "Industr_dev" = col.threats[3],
#             "Tourism" = col.threats[4],
#             "Sediments" = col.threats[5],
#             "Nutrients" = col.threats[6],
#             "Cumulative" = "darkgrey")
# 
# # Dot plot individual impacts
# 
# png("Figure top_threat_BCU.png",width=20, height=10, units="cm", res=300)
# a <- ggplot(ehe.arranged,aes(x=reorder(BCU_name,x.order))) +
#   geom_point(aes(y=fishing,colour="Fishing")) +
#   geom_point(aes(y=coast_dev,colour="Coastal\ndevelopment")) +
#   geom_point(aes(y=tourism,colour="Tourism")) +
#   geom_point(aes(y=sediments,colour="Sediments")) +
#   geom_point(aes(y=nutrients,colour="Nutrients")) +
#   scale_y_continuous(limits=c(0,1)) +
#   scale_color_manual(values = colors) +
#   labs(x = "",
#        y = "Percentile",
#        color = "Threat") +
#   # geom_vline(xintercept = sep.region, linetype="dashed", size = 0.25, show.legend=F)
# # facet_wrap(vars(Region),scales="free_x",nrow=1)
# facet_grid(cols = vars(Region), scales="free_x", space="free_x")
# print(a)
#   for (i.region in 1 : 9) {
#   a <- a +
#     annotation_custom(
#       grob = textGrob(label = region.names[i.region], hjust = 0.5, gp = gpar(cex = 0.5)),
#       ymin = region.ymin[i.region],
#       ymax = region.ymax[i.region],
#       xmin = region.xmin[i.region],
#       xmax = region.xmax[i.region])
#   }
# print(a)
# dev.off()