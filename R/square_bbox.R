#' Generate a square bounding box
#'
#' @param x Data to create bounding box for
#' @param ext Extension factor of the bounding box (relative to the data in \code{x}). A value greater than 1 means the bounding box will be larger than the bounds of the data, and a value less than 1 means the bounding box will be smaller.
#' @param output Type of output. Defaults to "bbox"
#'
#' @return A bounding box
#' @export
#'
#' @examples
#' bcu <- readRDS(here::here("data", "bcus_list.rds"))[["Singapore"]]
#' square_bbox(bcu)
square_bbox <- function(x, ext = 1.1, output = "bbox") {
  original_bbox <- tmaptools::bb(x)
  x_range <- original_bbox[["xmax"]] - original_bbox[["xmin"]]
  y_range <- original_bbox[["ymax"]] - original_bbox[["ymin"]]
  max_range <- max(x_range, y_range)
  max_range_ext <- max_range*ext
  tmaptools::bb(x, width = max_range_ext, height = max_range_ext, output = output)
}
