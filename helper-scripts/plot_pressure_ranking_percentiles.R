#' Plot top pressures percentiles
#'
#' Produce a ggplot2 plot showing the distribution of a BCU's pixels percentiles, relative to all reef pixels, and the percentile of the BCU's average, for all indicators (except number of ports, and excluding climate scores). The indicators are ordered in descending order by the value of the indicator average's percentile.
#'
#' @param bcu_percentiles Percentiles for a single BCU, from \code{data/bcus_percentiles.rds}
#' @param bcu_top_pressures Top pressures for a single BCU, from \code{data/bcus_top_pressures.rds}
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' bcu_name <- "Aceh"
#' bcu_percentiles <- readRDS(here::here("data", "bcus_percentiles.rds"))[[bcu_name]]
#' bcu_top_pressures <- readRDS(here::here("data", "bcus_top_pressures.rds"))[[bcu_name]]
#'
#' plot_pressure_ranking_percentiles(bcu_percentiles, bcu_top_pressures)
plot_pressure_ranking_percentiles <- function(bcu_percentiles, bcu_top_pressures) {

  bcu_top_pressures <- bcu_top_pressures %>%
    dplyr::filter(!stringr::str_detect(indicator_label, "Climate:"), indicator != "num_ports") %>%
    dplyr::mutate(indicator_label = stringr::str_replace(indicator_label, ": ", ":\n"))

  bcu_percentiles <- bcu_percentiles %>%
    dplyr::filter(!stringr::str_detect(indicator, "score"), indicator != "num_ports") %>%
    dplyr::filter(!is.na(percentile))

  bcu_percentiles_combined <- bcu_percentiles %>%
    dplyr::inner_join(bcu_top_pressures, by = c("bcu", "indicator"), suffix = c("", "_mean"))

  ggplot2::ggplot() +
    ggbeeswarm::geom_quasirandom(ggplot2::aes(x = reorder(indicator_label, rank), y = percentile, colour = percentile), data = bcu_percentiles_combined, alpha = 0.5) +
    ggplot2::geom_errorbar(ggplot2::aes(x = reorder(indicator_label, rank), y = percentile, ymin = percentile, ymax = percentile), data = bcu_top_pressures) +
    ggplot2::scale_colour_gradient2(
      low = low_blue, mid = mid_yellow, high = high_red,
      midpoint = 0.5, limits = c(0, 1)
    ) +
    ggplot2::labs(x = NULL, y = "Pressure Percentile",
         title = "Pressures ranked from highest to lowest; BCU average and pixels compared to all reef pixels",
         subtitle = "A value in the 50th percentile means that the BCU's average is higher than 50% of the world's coral reefs values",
         caption = "(Number of ports excluded from ranking due to consistently low values)") +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    ggplot2::scale_fill_gradient2(
      low = low_blue, mid = mid_yellow, high = high_red,
      midpoint = 0.5, limits = c(0, 1)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none", plot.title.position = "plot",
          plot.title = ggplot2::element_text(size = 11),
          plot.subtitle = ggplot2::element_text(size = 9))
}
