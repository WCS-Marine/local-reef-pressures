# Analysis of the results

rm(list=ls())

library(tidyverse)
library(sf)
library(here)
library(corrgram)

load(here("scripts","DataForAnalysis.RData"))

data <- allreefs_withBCU_prc_centroids

table(data$top.threat)
table(data$top.threat) / sum(table(data$top.threat))

hist(data$grav_NC[data$top.threat == 1])

corrgram(as.data.frame(data)[,vthreats[c(1,2,4,5,6)]],
         upper.panel=panel.cor,
         lower.panel = panel.pts, cor.method="spearman")


corrgram(as.data.frame(data)[,paste0(vthreats[c(1,2,4,5,6)],"_raw")],
         upper.panel=panel.cor,
         lower.panel = panel.pts, cor.method="spearman")

# Chi-square : observed top threats (in BCUs) vs expected (in all reefs)
load(here("scripts","TopThreat_RawValuePrc_BCUMedians_10_04.RData"))
exp.tt <- table(allreefs_withBCU_prc_centroids$top.threat)
obs.tt <- table(factor(top_threats_table$top.threat,levels=c(1:6)))
chisq.test(rbind(exp.tt, obs.tt))
chisq.test(obs.tt, p = exp.tt/sum(exp.tt))
chisq.test(obs.tt, p = exp.tt, rescale.p = T)
chisq.test(obs.tt, p = exp.tt, rescale.p = T, simulate.p.value = T)
