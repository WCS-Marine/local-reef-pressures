#' Generate Reef Threats report cards
#'
#' @param bcu BCU(s) to generate report cards for. Defaults to "all", to generate for all BCUs.
#' @param open Whether to open the PDF once the report card is generated. Defaults to TRUE.
#' @param quiet Whether to suppress messages about which report card is being produced and where it is saved. Defaults to FALSE.
#'
#' @return A set of pdf report cards saved in \code{analysis/report_cards/}
#' @export
generate_report_cards <- function(bcu = "all", open = TRUE, quiet = FALSE) {

  bcus <- readRDS(here::here("data", "report-cards", "bcus_list.rds"))
  bcus <- names(bcus)

  if (all(bcu == "all")) {
    bcu <- bcus
  } else if (!all(bcu %in% bcus)) {
    usethis::ui_stop("Can't generate report cards for all BCUs supplied right now.\nAvailable BCUs are {glue::glue_collapse(bcus, sep = ', ')}")
  }

  multiple_bcus <- length(bcu) > 1
  if (multiple_bcus) {
    source(here::here("scripts", "report-cards", "99-report_card_setup.R"))
  }

  purrr::walk(bcu, generate_single_report_card, open, quiet, standalone = !multiple_bcus)

}

#' Generate a single reef threats report card
#'
#' @param bcu A single BCU to generate report cards for
#' @param open Whether to open the PDF once the report card is generated. Defaults to TRUE
#' @param quiet Whether to suppress messages about which report card is being produced and where it is saved. Defaults to FALSE
#' @param standalone Whether the report card is being produced standalone (i.e., only one report card is being produced) - defaults to TRUE. This controls whether the libraries and data are loaded when the function is called.
#'
#' @return A pdf report card saved in \code{analysis/report_cards/}
#' @export
#'
#' @examples
#' generate_single_report_card("Eel - NGBR")
generate_single_report_card <- function(bcu, open = TRUE, quiet = FALSE, standalone = TRUE) {

  fs::dir_create(here::here("report_cards"))

  if (length(bcu) > 1) {
    stop("This function can only generate a report card for a single BCU.\nPlease use `generate_report_cards` to generate for multiple BCUs.")
  }

  bcu_country <- readRDS(here::here("data", "report-cards", "bcus_regions.rds"))[[bcu]][["country"]]
  bcu_name <- recode_bcus(bcu)

  if (!quiet) {
    usethis::ui_info("Rendering report card for {bcu_name} BCU...")
  }

  bcu_name <- stringr::str_replace_all(bcu_name, "/| ", "-")
  bcu_name <- stringr::str_replace_all(bcu_name, "---", "-")
  bcu_country <- stringr::str_replace_all(bcu_country, " ", "-")
  out_file <- here::here("report_cards", glue::glue("{bcu_country}_{bcu_name}.pdf"))

  rmarkdown::render(
    input = here::here("scripts", "report-cards", "99-report_card_template.Rmd"),
    output_file = out_file,
    params = list(reef = bcu,
                  standalone = TRUE),
    quiet = TRUE
  )

  if (!quiet) {
    usethis::ui_done("BCU report card saved in {usethis::ui_path(out_file)}")
  }

  if (open) {
    system2("open", paste0("'", out_file, "'"))
  }
}
