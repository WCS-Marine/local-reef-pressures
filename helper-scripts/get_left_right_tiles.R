#' Get left and right tiles
#'
#' Get left and right OSM tiles for a given BCU - necessary for BCUs that cross the 180th meridian line (i.e., Fiji BCUs), as \code{tmaptools::read_osm} uses a fixed projection that cannot be changed and so cuts off.
#'
#' @param bcus Indicator data for BCUs, from \code{data/bcus_list.rds}
#' @param bcu_name Name of BCU that tiles are being pulled for
#' @param bcu_ext Extension factor of the BCU's bounding box, from \code{data/bcus_ext.rds}
#' @param tile_type Type of OSM tile to get. Defaults to "bing".
#' @param zoom Tile zoom. Defaults to NULL, in which case it is determined automatically
#'
#' @return A named list ("left" and "right") of OSM tiles.
#' @export
#'
#' @examples
#' bcu_name <- "Vatu-i-Ra"
#' bcus <- readRDS(here::here("data", "bcus_list.rds"))
#' bcu_ext <- readRDS(here::here("data", "bcus_ext.rds"))[[bcu_name]]
#'
#' get_left_right_tiles(bcus, bcu_name, bcu_ext)
get_left_right_tiles <- function(bcus, bcu_name, bcu_ext, tile_type = "bing", zoom = NULL) {
  bcu <- bcus[[bcu_name]]
  bcu <- sf::as_Spatial(bcu)
  prj <- "+proj=longlat +datum=WGS84 +no_defs"

  bb.meters <- as.data.frame(t(square_bbox(bcu, ext = bcu_ext*1.5, output = "matrix")))
  colnames(bb.meters) <- c("lon", "lat")
  sp::coordinates(bb.meters) <- c("lon", "lat")
  sp::proj4string(bb.meters) <- sp::CRS(sp::proj4string(bcu))
  # Transform CRS of spatial points into loglat
  bb.longlat <- sp::spTransform(bb.meters, sp::CRS(prj))

  bb.left <- bb.right <- t(bb.longlat@coords)
  bb.left[1, 2] <- 180
  bb.left <- tmaptools::bb(bb.left)
  bb.right[1, 1] <- -180
  bb.right <- tmaptools::bb(bb.right)
  tile.left <- tmaptools::read_osm(bb.left, type = tile_type, zoom = zoom)
  tile.right <- tmaptools::read_osm(bb.right, type = tile_type, zoom = zoom)

  tile <- list(
      left = tile.left,
      right = tile.right
    )

  return(tile)
}
