#' Map BCU Indicator
#'
#' Produce a satellite map for a BCU, along with the values for a supplied indicator.
#'
#' @param bcu Indicator data for a single BCU, from \code{data/bcus_list.rds}
#' @param tile Bing tile for a single BCU, from \code{data/bcus_tiles_indicator.rds}
#' @param indicator Indicator being mapped
#' @param indicator_title Longer-form title for indicator being mapped, used in creating the map title. Defaults to \code{indicator}
#' @param bcu_ext Extension factor of the BCU's bounding box, from \code{data/bcus_ext.rds}
#'
#' @return A \code{tmap} object
#' @export
#'
#' @examples
#' bcu_name <- "Aceh"
#' bcu <- readRDS(here::here("data", "bcus_list.rds"))[[bcu_name]]
#' tile <- readRDS(here::here("data", "bcus_tiles_indicator.rds"))[[bcu_name]]
#' bcu_ext <- readRDS(here::here("data", "bcus_ext.rds"))[[bcu_name]]
#'
#' map_indicator(bcu, tile,
#'   indicator = "grav_NC",
#'   indicator_title = "Fishing: Market Pressure", bcu_ext
#' )
map_indicator <- function(bcu, tile, indicator = NA, indicator_title = indicator, bcu_ext) {
  if (inherits(tile, "list") & all(names(tile) == c("left", "right"))) {
    m <- tmap::tm_shape(tile[["left"]],
      bbox = square_bbox(bcu, ext = bcu_ext),
      projection = sf::st_crs(bcu)
    ) +
      tmap::tm_rgb() +
      tmap::tm_shape(tile[["right"]]) +
      tmap::tm_rgb()
  } else {
    m <- tmap::tm_shape(tile) +
      tmap::tm_rgb()
  }

  if (!is.na(indicator)) {
    indicator_breaks <- breaks[[indicator]]
    breaks_midpoint <- ceiling(length(indicator_breaks) / 2)
    breaks_midpoint_value <- indicator_breaks[[breaks_midpoint]]
    blank_breaks <- rep("", breaks_midpoint - 2)
    legend_labels <- c(min(indicator_breaks), blank_breaks, breaks_midpoint_value, blank_breaks, max(indicator_breaks))
    legend_style <- "cont"

    m <- m +
      tmap::tm_shape(bcu) +
      tmap::tm_fill(
        col = indicator, palette = RColorBrewer::brewer.pal(length(indicator_breaks), "OrRd"), style = legend_style, breaks = indicator_breaks, title = indicator_title, scale = 0.8, alpha = 1, legend.is.portrait = FALSE, labels = legend_labels, showNA = FALSE
      ) +
      tmap::tm_legend(show = FALSE)
  }

  m +
    tmap::tm_layout(
      main.title = indicator_title,
      main.title.position = "center",
      main.title.size = 0.75,
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    ) +
    tmap::tmap_options(
      output.dpi = 96,
      show.messages = FALSE
    )
}

legend_titles <- list(
  grav_NC = "Market gravity",
  pop_count = "Number of individuals",
  num_ports = "Number of ports",
  reef_value = "USD (thousands)",
  sediment = "ton / km2",
  nutrient = "ton / km2"
)

#' Generate legend for a BCU indicator map
#'
#' Generate a ggplot2 legend for a BCU indicator map. The ggplot2 legend is more flexible than the tmap legend, so a ggplot2 legend is produced for every map from \code{map_indicator}.
#'
#' @param indicator Indicator being mapped
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' generate_map_legend("grav_NC")
generate_map_legend <- function(indicator) {
  legend_breaks <- breaks[[indicator]]
  legend_n <- length(legend_breaks)

  df <- dplyr::tibble(legend_breaks = legend_breaks) %>%
    dplyr::mutate(id = dplyr::row_number())

  p <- ggplot2::ggplot(df, ggplot2::aes(x = id, y = legend_breaks, colour = legend_breaks)) +
    ggplot2::geom_point() +
    ggplot2::guides(col = ggplot2::guide_legend(override.aes = list(shape = 15, size = 5), label.position = "bottom", title.position = "top"))


  breaks_midpoint <- ceiling(length(legend_breaks) / 2)
  breaks_midpoint_value <- legend_breaks[[breaks_midpoint]]
  actual_midpoint_value <- max(legend_breaks) / 2
  actual_legend_breaks <- c(min(legend_breaks), actual_midpoint_value, max(legend_breaks))
  legend_labels <- c(min(legend_breaks), breaks_midpoint_value, max(legend_breaks))

  p <- p +
    ggplot2::scale_colour_gradientn(
      colours = RColorBrewer::brewer.pal(length(legend_breaks), "OrRd"),
      breaks = actual_legend_breaks,
      labels = scales::comma(legend_labels, accuracy = 1)
    ) +
    ggplot2::guides(colour = ggplot2::guide_colourbar(ticks = FALSE, title.position = "top"))

  p <- p +
    ggplot2::labs(colour = legend_titles[[indicator]]) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(margin = ggplot2::margin(t = -2.5)),
      legend.key = ggplot2::element_blank(),
      legend.title.align = 0.5
    )

  l <- ggpubr::get_legend(p)
  ggpubr::as_ggplot(l)
}

breaks <- list(
  grav_NC = c(0, 10^(-1:6)),
  pop_count = c(0, 10^(2:7)),
  num_ports = 0:6,
  reef_value = c(0, 2.5 * 10^(0:5)),
  sediment = c(0, 10^(1:4)),
  nutrient = c(0, 10^(-2:5))
)


layout_indicator_maps <- function(bcu, tile_indicator, bcu_ext) {
  size <- 600
  width_in <- size / 300
  height_in <- width_in * 0.3

  indicator_with_title <- dplyr::tribble(
    ~indicator, ~title,
    "grav_NC", "Fishing:\nMarket Pressure",
    "sediment", "Pollution:\nSedimentation",
    "nutrient", "Pollution:\nNutrients",
    "pop_count", "Coastal Development:\nHuman Population",
    "num_ports", "Industrial Development:\nPorts",
    "reef_value", "Tourism:\nReef Value"
  )

  tmap_dir <- tempdir()

  purrr::walk2(
    indicator_with_title[["indicator"]], indicator_with_title[["title"]],
    function(indicator, title) {
      map_res <- map_indicator(bcu, tile_indicator, indicator, title, bcu_ext)
      tmap::tmap_save(map_res, glue::glue("{tmap_dir}/{indicator}.png"), width = size, height = size, dpi = 300)

      legend_res <- generate_map_legend(indicator)
      ggsave(glue::glue("{tmap_dir}/{indicator}_legend.png"), legend_res, width = width_in, height = height_in, units = "in")
    }
  )

  tmaps <- indicator_with_title[["indicator"]] %>%
    purrr::map(~ magick::image_read(glue::glue("{tmap_dir}/{.x}.png")))
  names(tmaps) <- indicator_with_title[["indicator"]]

  legends <- indicator_with_title[["indicator"]] %>%
    purrr::map(~ magick::image_read(glue::glue("{tmap_dir}/{.x}_legend.png")))
  names(legends) <- indicator_with_title[["indicator"]]

  panel_size <- magick::image_info(tmaps[[1]])[["height"]]
  legend_height <- magick::image_info(legends[[1]])[["height"]]

  plot_image <- magick::image_blank(width = panel_size * 3, height = panel_size * 2 + legend_height * 2, color = "white")

  width_offset <- 0
  height_offset <- panel_size

  for (i in 1:3) {
    plot_image <- magick::image_composite(plot_image, tmaps[[i]], offset = glue::glue("+{width_offset}+0"))
    plot_image <- magick::image_composite(plot_image, legends[[i]], offset = glue::glue("+{width_offset}+{height_offset}"))
    # Then increment the offset by the width of the panel, so the next panel can use it
    width_offset <- width_offset + panel_size
  }

  # Add the last 3 panels

  width_offset <- 0
  height_offset <- panel_size + legend_height
  for (i in 4:6) {
    plot_image <- magick::image_composite(plot_image, tmaps[[i]], offset = glue::glue("+{width_offset}+{height_offset}"))
    plot_image <- magick::image_composite(plot_image, legends[[i]], offset = glue::glue("+{width_offset}+{height_offset + panel_size}"))
    width_offset <- width_offset + panel_size
  }

  final_file <- glue::glue("{tmap_dir}/indicators_map.png")

  magick::image_write(plot_image, final_file, quality = 72)

  return(final_file)
}
