# Other analysis
# 13/02/2021

rm(list=ls())

library(tidyverse)
library(sf)
library(here)
library(RColorBrewer)


load(here("scripts","DataForAnalysis.RData"))
allreefs_withRegion <- read_sf(paste0(getwd(),"/data"),"allreefs_withRegion")
allreefs_withBCU_prc_centroids$Region <- allreefs_withRegion$REEFTHR
rm(allreefs_withRegion)



# Statistical comparison between regions
library(dunn.test)
library(xlsx)
threats$score.l <- allreefs_withBCU_prc_centroids$score.l
threats$Region <- factor(allreefs_withBCU_prc_centroids$Region)


for (i in 1 : 7) {
  indicator <- names(threats)[i]
  cat(indicator,"KW p.value",kruskal.test(threats[[indicator]]~threats$Region)$p.value,"\n")
  rdunn <- dunn.test(threats[[indicator]],threats$Region,method="holm",kw=F,wrap=T)
  if (i == 1) {
    res <- data.frame(comp=rdunn$comparisons, grav_NC=rdunn$P.adjust)
  } else {
    res[[indicator]] <- rdunn$P.adjust
  }
}
write.xlsx(res, file="Results_Dunn.xlsx", row.names=F)

dunn.test(threats$pop_count,threats$Region,method="holm")
dunn.test(threats$reef_value,threats$Region,method="holm")
dunn.test(threats$sediment,threats$Region,method="holm")
dunn.test(threats$nutrient,threats$Region,method="holm")
dunn.test(threats$score.l,threats$Region,method="holm")

tapply(threats$grav_NC,threats$Region,function(x){shapiro.test(x)$p.value})
# end here

