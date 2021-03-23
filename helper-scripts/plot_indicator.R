#' Plot indicator values
#'
#' Produce a ggplot2 plot of a BCU's indicator values, along with the global median for that indicator and the medians of other BCUs in that subregion.
#'
#' @param bcus_top_threats Top threats for all BCUs, from \code{data/bcus_top_threats.rds}
#' @param bcu_name Name of the BCU being plotted
#' @param indicator Indicator being plotted
#' @param bcus_regions Data containing which regions BCUs belong to, from \code{data/bcus_regions.rds}
#'
#' @return
#' @export
#'
#' @examples
#' bcus_top_threats <- readRDS(here::here("data", "bcus_top_threats.rds"))
#' bcus_regions <- readRDS(here::here("data", "bcus_regions.rds"))
#'
#' plot_indicator_dumbbell(bcus_top_threats, bcus_regions, "Aceh", "grav_NC")
plot_indicator_dumbbell <- function(bcus_top_threats, bcus_regions, bcu_name, indicator) {

  bcus_top_threats <- dplyr::bind_rows(bcus_top_threats)
  bcus_regions <- dplyr::bind_rows(bcus_regions)

  indicator_data <- bcus_top_threats %>%
    dplyr::filter(indicator == !!indicator)

  indicator_median <- unique(indicator_data[["median"]])

  bcu_data <- indicator_data %>%
    dplyr::filter(bcu == bcu_name) %>%
    dplyr::mutate(
      indicator_label = stringr::str_replace(indicator_label, ": ", ":\n"),
      meaning = "Global Average"
    )

  region <- bcus_regions %>%
    dplyr::filter(bcu == bcu_name) %>%
    dplyr::pull(subregion)

  region_data <- indicator_data %>%
    dplyr::left_join(bcus_regions %>% dplyr::select(bcu, subregion), by = "bcu") %>%
    dplyr::filter(
      subregion == region,
      bcu != bcu_name
    ) %>%
    dplyr::mutate(subregion = ifelse(subregion == "Australia and New Zealand", "Australia", subregion),
      indicator_label = stringr::str_replace(indicator_label, ": ", ":\n"),
      meaning = paste("Other", subregion, "BCU Averages")
    )

  region_range <- indicator_data %>%
    dplyr::left_join(bcus_regions %>% dplyr::select(bcu, subregion), by = "bcu") %>%
    dplyr::filter(subregion == region) %>%
    dplyr::pull(reef_median) %>%
    range()

  if (indicator %in% c(
    "grav_NC", "sediment", "nutrient",
    "pop_count", "num_ports", "reef_value", "num_ports"
  )) {
    plot_range <- c(0, max(region_range[[2]], indicator_median))
    plot_range <- decimal_ceiling(plot_range)
    label_scale <- scales::comma_format(accuracy = 1)
  } else if (stringr::str_detect(indicator, "score")) {
    plot_range <- c(decimal_floor(min(region_range[[1]], indicator_median)), decimal_ceiling(max(region_range[[2]], indicator_median)))
    label_scale <- scales::number_format(accuracy = 0.1)
  }

  ggplot2::ggplot() +
    # Other in region
    ggplot2::geom_jitter(ggplot2::aes(x = reef_median, y = indicator_label, fill = meaning), colour = "grey", height = 0.10, width = 0, data = region_data, size = 3, alpha = 0.60) +
    # "Global"
    ggplot2::geom_jitter(ggplot2::aes(x = median, y = indicator_label, shape = meaning), colour = "black", height = 0.05, width = 0, data = bcu_data, size = 3, alpha = 0.60) +
    ggplot2::scale_shape_manual(values = "triangle") +
    # Current ID
    ggplot2::geom_jitter(ggplot2::aes(x = reef_median, y = indicator_label, colour = percentile), height = 0.05, width = 0, data = bcu_data, size = 3, alpha = 0.60) +
    ggplot2::scale_x_continuous(limits = plot_range, labels = label_scale) +
    ggplot2::scale_colour_gradient2(
      low = low_blue, mid = mid_yellow, high = high_red,
      midpoint = 0.5, limits = c(0, 1),
      breaks = c(0, 0.5, 1), labels = c("Low", " ", "High")
    ) +
    ggplot2::labs(x = NULL, y = NULL, subtitle = indicator_data[["indicator_label"]]) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      legend.justification = "left"
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_colourbar(ticks = FALSE, barwidth = 7, order = 3),
      shape = ggplot2::guide_legend(label.position = "bottom", order = 1, reverse = TRUE),
      fill = ggplot2::guide_legend(label.position = "bottom", order = 2)
    )
}

low_blue <- "#324cd6"
mid_yellow <- "#ffd700"
high_red <- "#eb3232"

decimal_ceiling <- function(x, digits = 1) {
  x2 <- x * 10^digits
  ceiling(x2) / 10^digits
}

decimal_floor <- function(x, digits = 1) {
  x2 <- x * 10^digits
  floor(x2) / 10^digits
}
