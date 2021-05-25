source(here::here("R", "generate_report_card.R"))
source(here::here("R", "recode_bcus.R"))

generate_report_cards(bcu = "all", open = FALSE, quiet = TRUE)
