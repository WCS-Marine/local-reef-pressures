#' Recode BCUs
#'
#' Clean BCU names for consistency.
#'
#' @param bcu A vector of BCU names
#'
#' @return A vector of BCU names - cleaned if needed, otherwise just returns the original BCU name.
#' @export
#'
#' @examples
#' recode_bcus("Eel - NGBR")
#' # [1] "Eel - Northern GBR"
#' recode_bcus("North Red Sea-Egypt II")
#' # [1] "North Red Sea - Egypt II"
#' recode_bcus("Bahamas")
#' # [1] Bahamas
recode_bcus <- function(bcu) {
  dplyr::recode({{ bcu }},
    `Eel - NGBR` = "Eel - Northern GBR",
    `Mason - NGBR` = "Mason - Northern GBR",
    `WhitsundayReef 1 - SGBR` = "Whitsunday Reef 1 - Southern GBR",
    `WhitsundayReef 2-SGBR` = "Whitsunday Reef 2 - Southern GBR",
    `Great Barrier Reef Central` = "GBR Central",
    `Gallon-NGBR` = "Gallon - Northern GBR",
    `Mackay-GBR` = "Mackay - GBR",
    `North Red Sea-Egypt II` = "North Red Sea - Egypt II",
    `Takabonarte` = "Taka Bonarte"
  )
}
