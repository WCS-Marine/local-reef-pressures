# Analysis of the results

rm(list=ls())

library(tidyverse)
library(sf)
library(here)
library(corrgram)
library(RColorBrewer)
library(prettyR)

load(here("scripts","DataForAnalysis.RData"))

threat_names <- c("Fishing","Coastal dev","Industrial dev",
                  "Tourism","Sediments","Nutrients", "Cumulative impact score" )

# Colors: colorblind friendly palette
col_threats <- brewer.pal(6,"Set2")
colors <- c("Fishing" = col_threats[1],
            "Coastal\ndevelopment" = col_threats[2],
            "Industr_dev" = col_threats[3],
            "Tourism" = col_threats[4],
            "Sediments" = col_threats[5],
            "Nutrients" = col_threats[6],
            "Cumulative" = "darkgrey")
# Titles
title.text <- c("Fishing","Coastal\ndevelopment","Industrial\ndevelopment",
                "Tourism","Sediments","Nutrients","Cumulative")

data <- allreefs_withBCU_prc

# Global medians
glob.median <- vector()
for (i in 1 : length(vthreats)) {
  indicator <- vthreats[i]
  glob.median[i] <- median(as.data.frame(data)[,indicator],na.rm=T)
}
names(glob.median) <- vthreats
glob.median[7] <- median(data$score.l)

# Theme
theme_set(theme_minimal(10))
theme_update(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
             axis.title = element_blank(),
             legend.text = element_text(size=7),
             legend.key.size = unit(.5, "cm"),
             legend.position = "bottom",
             plot.title = element_text(hjust = 0.5, size = 10))

# Individual threats: regional comparisons
for (i in  1 : 7) {
  if (i < 7) indicator <- vthreats[i] else indicator <- "score.l"
  png(paste0("plots/boxplots/Boxplot_",indicator,".png"), width=4, height=10, units="cm", res=300)
  a <- ggplot(data,aes(x=reorder(Region,!!sym(indicator),na.rm=T),y=!!sym(indicator))) +
    geom_boxplot(fill=colors[i], size=0.1, outlier.size = 0.1, show.legend=F) +
    geom_hline(aes(yintercept = glob.median[i]), linetype="dashed", size = 0.25, show.legend=F) +
    scale_y_continuous(limits=c(0,1)) +
    labs(title=title.text[i])
  print(a)
  dev.off()
}
# Composed in powerpoint and saved as Figure S7

# Top threats
theme_update(axis.text.y = element_text(hjust = 1, vjust = 0.5, size = 7),
             axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 7))
table(data$top.threat)
table(data$top.threat) / sum(table(data$top.threat))
# Comparison of top-threats between regions
ta <- as.data.frame(table(allreefs_withBCU_prc_centroids$Region,
                          allreefs_withBCU_prc_centroids$top.threat,
                          dnn=c("Region","threat")))
ta$top_threat <- threat_names[ta$threat]
ta$top_threat <- factor(ta$top_threat, levels=threat_names)

png(paste0("plots/Figure S8.png"), width=10, height=8, units="cm", res=300)
a<-ggplot(ta,aes_string(y="Region",x="Freq",fill="top_threat")) +
  geom_col(position=position_fill(reverse=T)) + 
  scale_fill_manual(values=col_threats, name="")
print(a)
dev.off()

ta$n.reef <- table(data$Region)[ta$Region]
ta$Freq.rel <- ta$Freq / ta$n.reef

ta$Freq.rel[ta$top_threat=="Fishing"]
range(ta$Freq.rel[ta$threat==5]+ta$Freq.rel[ta$threat==6])

ta$Freq.rel[ta$Region=="Micronesia"]
ta$Freq.rel[ta$Region=="Melanesia"]
0.38376741+0.08407026

# Threat intensity
# Build a dataframe where each reef pixel has the value of the threat that is top-ranked
a <- data.frame(
  value = as_tibble(data) %>% filter(top.threat == 1) %>% select(!!sym(vthreats[1])) %>% as_vector(),
threat = vthreats[1]
)
for (i in 2 : 6) {
  a <- rbind(a,
             data.frame(
               value = as_tibble(data) %>% filter(top.threat == i) %>% select(!!sym(vthreats[i])) %>% as_vector(),
               threat = vthreats[i]
             )
  )
}

# Boxplots of the threat values that are top ranked
theme_update(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7))

png(paste0("plots/Figure S9.png"), width=5, height=10, units="cm", res=300)
a.plot <- ggplot(a,aes_string(y="value",x="threat",fill="threat")) +
  geom_boxplot(size=0.1, outlier.size = 0.1, show.legend = F) + 
  scale_y_continuous(limits=c(0,1)) +
  scale_x_discrete(labels=threat_names) +
  scale_fill_manual(values=col_threats, name="")
print(a.plot)
dev.off()
tapply(a$value, a$threat, summary)



# Compare  top threats in BCUs vs non-BCUs
# Frequency of top-ranked threats
ta.bcu <- as.data.frame(table(data$is.bcu,
                          data$top.threat,
                          dnn=c("BCU","threat")))

ta.bcu$top_threat <- threat_names[ta.bcu$threat]
ta.bcu$top_threat <- factor(ta.bcu$top_threat, levels=threat_names)
png(paste0("plots/Figure S10.png"), width=10, height=5, units="cm", res=300)
a<-ggplot(ta.bcu,aes_string(y="BCU",x="Freq",fill="top_threat")) +
  geom_col(position=position_fill(reverse=T)) + 
  scale_fill_manual(values=col_threats, name="")
print(a)
dev.off()
nonBCU.tt <- table(data$top.threat[data$is.bcu=="non BCUs"])
BCU.tt <- table(data$top.threat[data$is.bcu=="BCUs"])
chisq.test(rbind(nonBCU.tt, BCU.tt))
# chisq.test(obs.tt, p = exp.tt/sum(exp.tt))
# chisq.test(obs.tt, p = exp.tt, rescale.p = T)
# chisq.test(obs.tt, p = exp.tt, rescale.p = T, simulate.p.value = T)

# Distribution of threat percentiles
data1 <- as.data.frame(data)[,c("is.bcu",vthreats)]
data2 <- rep_n_stack(data1, to.stack=vthreats, stack.names=c("indicator","value"))
data2$indicator <- factor(data2$indicator, levels=vthreats) # reorder levels
data2$title.text <- factor(title.text[data2$indicator], levels = title.text)
theme_update(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 7),
             axis.text.y = element_blank())
png(paste0("plots/Figure 5.png"), width=18.5, height=13, units="cm", res=300)
a <- ggplot(data2,aes(x=value, fill=is.bcu)) +
  geom_density(na.rm=T, alpha=0.5) +
  facet_wrap(vars(title.text), scales="free_y") +
  scale_fill_brewer(name="", type="qual")
print(a)
dev.off()
wilcox.test(data$grav_NC~data$is.bcu)
wilcox.test(data$pop_count~data$is.bcu)
wilcox.test(data$num_ports~data$is.bcu)
wilcox.test(data$reef_value~data$is.bcu)
wilcox.test(data$sediment~data$is.bcu)
wilcox.test(data$nutrient~data$is.bcu)
# Boxplot:
# theme_update(plot.title = element_text(hjust = 0.5, size = 10),
#              plot.margin = margin(0.2,0.2,0.2,0.2,"cm"))
# for (i in 1 : length(vthreats)) {
#   # if (i == 3) next
#   indicator <- vthreats[i]
#   png(paste0("plots/Boxplot_BCU_",indicator,".png"), width=3, height=10, units="cm", res=300)
#   a <- 
#     ggplot(data, aes_string(x="is.bcu",y=indicator)) +
#     geom_boxplot(fill=colors[i], size=0.1, outlier.size = 0.1, show.legend=F) +
#     scale_y_continuous(limits=c(0,1)) +
#     labs(title=title.text[i], y="")
#   print(a)
#   dev.off()
# }

# I STOP HERE - 13TH FEB 2021


# Correlation among threats
corrgram(as.data.frame(data)[,vthreats[c(1,2,4,5,6)]],
         upper.panel=panel.cor,
         lower.panel = panel.pts, cor.method="spearman")


corrgram(as.data.frame(data)[,paste0(vthreats[c(1,2,4,5,6)],"_raw")],
         upper.panel=panel.cor,
         lower.panel = panel.pts, cor.method="spearman")


