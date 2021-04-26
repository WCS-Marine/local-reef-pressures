# Generate percentile plot for supplementary methods
library(ggplot2)

source(here::here("scripts", "report-cards", "99-report_card_setup.R"))

bcu <- "Northern Papua"

p <- plot_pressure_ranking_percentiles(bcus_percentiles[[bcu]], top_pressures[[bcu]])

ggsave("supplementary_methods_beeswarm_plot.png", p, width = 7, height = 4, dpi = 300)

p_no_caption <- p +
  theme(plot.caption = element_blank())

ggsave("supplementary_methods_beeswarm_plot_no_caption.png", p_no_caption, width = 7, height = 4, dpi = 300)
