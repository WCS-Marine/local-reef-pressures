# Analysis of the results

# Code to:
# - perform all the analyses of the article
# - draw Figure 3 (Individual pressures: regional comparisons)
# - draw Figure 4 (Density distribution of pressure percentiles and frequency of occurrence of top pressures in refugia vs non-refugia)
# - draw Figure S1 (Density distribution of pressure raw values)
# - draw Figure S2 (Correlation among pressures)
# - draw Figure S10 (Comparison of frequency of occurrence of top pressures between regions)
# - draw Figure S11 (Pressure intensity when top-ranked)

rm(list = ls())

library(tidyverse)
library(sf)
library(here)
library(corrgram)
library(RColorBrewer)
library(prettyR)

load(here("data", "allreefs.RData"))

# Names of the six pressure plus cumulative impact score in the columns of allreefs
vthreats <- c(
  "grav_NC", "pop_count", "num_ports",
  "reef_value", "sediment", "nutrient", "cumul_score"
)

# Names of the six pressures plus cumulative impact score
threat_names <- c(
  "Fishing", "Coastal pop", "Industrial dev",
  "Tourism", "Sediments", "Nitrogen", "Cumulative impact score"
)

# Colors: colorblind friendly palette
col_threats <- brewer.pal(6, "Set2")
colors <- c(
  "Fishing" = col_threats[1],
  "Coastal\npopulation" = col_threats[2],
  "Industr_dev" = col_threats[3],
  "Tourism" = col_threats[4],
  "Sediments" = col_threats[5],
  "Nitrogen" = col_threats[6],
  "Cumulative" = "darkgrey"
)
# Titles
title.text <- c(
  "Fishing", "Coastal\npopulation", "Industrial\ndevelopment",
  "Tourism", "Sediments", "Nitrogen", "Cumulative"
)

# Just change the name to be more comfortable
data <- allreefs
rm(allreefs)

# Calculate global medians
glob.median <- vector()
for (i in 1:length(vthreats)) {
  indicator <- vthreats[i]
  glob.median[i] <- median(as.data.frame(data)[, indicator], na.rm = T)
}
names(glob.median) <- vthreats


# Theme
ggplot2::theme_set(theme_minimal(10))
ggplot2::theme_update(
  axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 7),
  axis.title = element_blank(),
  legend.text = element_text(size = 7),
  legend.key.size = unit(.5, "cm"),
  legend.position = "bottom",
  plot.title = element_text(hjust = 0.5, size = 10)
)

# MAIN RESULTS
# Frequency of occurrence of top pressures
table(data$top_threat)
table(data$top_threat) / sum(table(data$top_threat))



#######################################################

# FIGURE 3. Individual pressures: regional comparisons

for (i in 1:7) {
  if (i < 7) indicator <- vthreats[i] else indicator <- "cumul_score"
  png(paste0("Boxplot_", indicator, ".png"), width = 10, height = 4, units = "cm", res = 300)
  a <- 
    
    # This plots regions ordered by their median value:
    # ggplot2::ggplot(data, aes(y = reorder(Region, !!sym(indicator), FUN = median, na.rm = T), x = !!sym(indicator))) +
    
    # This plots regions in alphabetical order:
    ggplot2::ggplot(data, aes(y = Region, x = !!sym(indicator))) +
    
    ggplot2::geom_boxplot(fill = colors[i], size = 0.1, outlier.size = 0.1, show.legend = F) +
    ggplot2::geom_vline(aes(xintercept = glob.median[i]), linetype = "dashed", size = 0.25, show.legend = F) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::labs(title = title.text[i])
  print(a)
  dev.off()
}
rm(a, i, indicator)
# Composed in powerpoint and saved as Figure 3



#######################################################

# FIGURE 4 (top). Density distribution of pressure percentiles in refugia vs non-refugia

# Retain only pressure percentiles and stack all pressure + refugia/non-refugia data in the same dataframe
data1 <- as.data.frame(data)[, c("is.bcu", vthreats[1:6])]
data2 <- prettyR::rep_n_stack(data1, to.stack = vthreats[1:6], stack.names = c("indicator", "value"))

# Reorder levels of pressures
data2$indicator <- factor(data2$indicator, levels = vthreats[1:6])

# Add title text (pretty name for pressures)
data2$title.text <- factor(title.text[data2$indicator], levels = title.text[1:6])

# Plot Figure 4 top
ggplot2::theme_update(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 7)) # ,
# axis.text.y = element_blank())
png(paste0("Figure 4_top.png"), width = 18.5, height = 13, units = "cm", res = 300)
a <- ggplot2::ggplot(data2, aes(x = value, fill = is.bcu)) +
  ggplot2::geom_density(na.rm = T, alpha = 0.5) +
  ggplot2::facet_wrap(vars(title.text), scales = "free_y") +
  ggplot2::scale_fill_brewer(name = "", type = "qual")
print(a)
dev.off()
rm(data1, data2, a)



#######################################################

# FIGURE 4 (bottom). Comparison of frequency of occurrence of top pressures in refugia vs non-refugia

ggplot2::theme_update(
  axis.text.y = element_text(hjust = 1, vjust = 0.5, size = 7),
  axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 7)
)

# Calculate the frequency of occurrence of each pressure as top ranked in refugia vs non refugia
ta.bcu <- as.data.frame(table(data$is.bcu,
                              data$top_threat,
                              dnn = c("refugia", "threat")
))

# Add pressure names as factors
ta.bcu$top_threat <- threat_names[ta.bcu$threat]
ta.bcu$top_threat <- factor(ta.bcu$top_threat, levels = threat_names)
ta.bcu

# Plot Figure 4 (bottom)
png(paste0("Figure 4_bottom.png"), width = 10, height = 3.5, units = "cm", res = 300)
a <- ggplot2::ggplot(ta.bcu, aes_string(y = "refugia", x = "Freq", fill = "top_threat")) +
  ggplot2::geom_col(position = position_fill(reverse = T)) +
  ggplot2::scale_fill_manual(values = col_threats, name = "")
print(a)
dev.off()

# In power point, compose with Figure_4_top to create Figure 4

# # Chi-square tests to compare the frequency of occurrence of each pressure as top-ranked in refugia vs non-refugia
# nonBCU.tt <- table(data$top_threat[data$is.bcu == "non-refugia"])
# BCU.tt <- table(data$top_threat[data$is.bcu == "refugia"])
# chisq.test(rbind(nonBCU.tt, BCU.tt))
# rm(ta.bcu, a, nonBCU.tt, BCU.tt)




#######################################################

# FIGURE S1 - Density distribution of pressure raw values

# Retain only pressure raw values
vthreats_raw <- paste0(vthreats[1:6],"_raw")
data1 <- as.data.frame(data)[, c("OBJECTID",vthreats_raw)]

# Update graphics layout
ggplot2::theme_update(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 7),
                      axis.title = element_blank())

# Loop on the six pressures
quantiles <- array(NA,dim=c(6,6))
for (i.threat in 1 : 6) {
  
  # Define pressure name
  indicator <- vthreats_raw[i.threat]
  
  # Retain only the values of that pressure
  data_indicator <- data1[,c("OBJECTID",indicator)]
  
  # Set the transformation: identity (for num_ports) or log10(x+1) for all the others
  if (indicator == "num_ports_raw") {
    data_indicator$value <- data_indicator[,2]
    transformation <- "identity"
  } else {
    data_indicator$value <- data_indicator[,2]+1
    transformation <- "log10"
  }
  
  # Calculate quantiles
  quantiles_ithreat <- quantile(data_indicator$value,seq(0.1, 0.9, 0.2),na.rm=T)
  
  # Plot density distribution with quantiles
  png(paste0("Figure S1_",indicator,".png"), width = 6, height = 6, units = "cm", res = 300)
  a <- ggplot(data_indicator, aes(x=value)) +
    geom_density(na.rm = T) +
    scale_x_continuous(trans=transformation) +
    geom_vline(xintercept=quantiles_ithreat, colour=c("red","purple","blue","purple","red"), linetype=2,alpha=0.5) +
    ggtitle(title.text[i.threat])
  print(a)
  dev.off()
  
  # Calculate quantiles in the original scale
  quantiles[i.threat,] <- quantile(data_indicator[,2],
                                   c(0.1,0.3,0.5,0.7,0.9,1),
                                   na.rm=T)
}
colnames(quantiles) <- c("10%","30%","50%","70%","90%","100%")
rownames(quantiles) <- title.text[1:6]
rownames(quantiles)[2] <- "Coastal population"
rownames(quantiles)[3] <- "Industrial development"
round(quantiles,2)

# Composed in power point and saved as Figure S1



#######################################################

# FIGURE S2 - Correlation among pressures

png(paste0("Figure S2.png"), width = 12, height = 10, units = "cm", res = 300)
data_corr <- as.data.frame(data)[,vthreats[c(1:6)]]
names(data_corr) <- threat_names[1:6]
corrgram::corrgram(data_corr,
                   upper.panel=panel.cor,
                   lower.panel = NULL, cor.method="spearman")
dev.off()
rm(data_corr)



#######################################################

# FIGURE S10. Comparison of frequency of occurrence of top pressures between regions

ggplot2::theme_update(axis.text.y = element_text(hjust = 1, vjust = 0.5, size = 7))

# ta data frame gives how many reef cells have a given top pressure in each region
ta <- as.data.frame(table(data$Region,
  data$top_threat,
  dnn = c("Region", "threat")
))
ta$top_threat <- threat_names[ta$threat]
ta$top_threat <- factor(ta$top_threat, levels = threat_names)

# Plot Figure S10
png(paste0("Figure S10.png"), width = 10, height = 8, units = "cm", res = 300)
a <- ggplot2::ggplot(ta, aes_string(y = "Region", x = "Freq", fill = "top_threat")) +
  ggplot2::geom_col(position = position_fill(reverse = T)) +
  ggplot2::scale_fill_manual(values = col_threats, name = "")
print(a)
dev.off()

# Calculate relative frequency of occurrence of each pressure as top ranked
ta$n.reef <- table(data$Region)[ta$Region]
ta$Freq.rel <- ta$Freq / ta$n.reef

# Percent of reef cells where fishing is a top pressure, for each region
ta[ta$top_threat == "Fishing", c("Region", "Freq.rel")]
# Percent of reef cells where water pollution (nutrients + sediments) is a top pressure, for each region
data.frame(
  Region = levels(ta$Region),
  water.pollution = as.numeric(ta$Freq.rel[ta$threat == 5] + ta$Freq.rel[ta$threat == 6])
)

# Format table for csv file
ta <- ta[,c(1,5,4,3,6)]
names(ta) <- c("Region","num_pixel","pressure","count","frequency")
summary(ta)
write.csv(ta,file="Top_pressure_per_region.csv")
rm(a, ta)



#######################################################

# FIGURE S11. Pressure intensity when top-ranked

# Build a dataframe where each reef cell has the value of the threat that is top-ranked
a <- data.frame(
  value = tibble::as_tibble(data) %>% dplyr::filter(top_threat == 1) %>% dplyr::select(!!sym(vthreats[1])) %>% as_vector(),
  threat = vthreats[1]
)
for (i in 2:6) {
  a <- rbind(
    a,
    data.frame(
      value = tibble::as_tibble(data) %>% dplyr::filter(top_threat == i) %>% dplyr::select(!!sym(vthreats[i])) %>% as_vector(),
      threat = vthreats[i]
    )
  )
}

a$threat <- factor(a$threat, levels=c("grav_NC","pop_count","num_ports","reef_value","sediment","nutrient"))

# Boxplots of the pressure percentiles that are top ranked
ggplot2::theme_update(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 7))
png(paste0("Figure S11.png"), width = 10, height = 5, units = "cm", res = 300)
a.plot <- 
  ggplot2::ggplot(a, aes_string(x = "value", y = "threat", fill = "threat")) +
  ggplot2::geom_boxplot(size = 0.1, outlier.size = 0.1, show.legend = F) +
  ggplot2::scale_x_continuous(limits = c(0, 1)) +
  ggplot2::scale_y_discrete(labels = threat_names) +
  ggplot2::scale_fill_manual(values = col_threats, name = "")
print(a.plot)
dev.off()

# Summarize the statistics of each pressure when it is top-ranked
tapply(a$value, a$threat, summary)
rm(a, a.plot, i)
