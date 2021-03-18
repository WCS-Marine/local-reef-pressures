source(here::here("helper-scripts", "generate_report_card.R"))
source(here::here("helper-scripts", "recode_bcus.R"))

generate_report_cards(bcu = "all", open = FALSE, quiet = TRUE)