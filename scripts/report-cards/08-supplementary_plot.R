# Generate percentile plot for supplementary methods
library(ggplot2)

source(here::here("scripts", "report-cards", "99-report_card_setup.R"))

bcu <- "Northern Papua"

p <- plot_threat_ranking_percentiles(bcus_percentiles[[bcu]], top_threats[[bcu]])

ggsave("supplementary_methods_beeswarm_plot.png", p, width = 7, height = 4, dpi = 300)
