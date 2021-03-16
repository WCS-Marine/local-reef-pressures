#' Extract values of indicators
#'
#' @param data Dataframe containing the coordinates of the points to extract values for. It must be a four-column dataframe.
#' The 1st and 2nd columns are ignored by the function; they can contain the region name and the site name, for example.
#' Longitude must be on the 3rd column and latitude on the 4th column.
#' @param allreefs Object of type sp::SpatialPolygonsDataFrame containing polygons for all reefs with values for all indicators.
#' @param max.radius Maximum radius (in meters) to search for nearest polygons. See details.
#'
#' @return A dataframe. The first four columns are the same as in \code{data}, and the following columns contain the values of the indicators
#'
#' @details The function will first transform the input coordinates into the same coordinate reference system of the \code{allreefs} object,
#' which follows a WGS84 Pacific-centered reference system (EPSG code: 3832). For each input point, it will then search for the nearest centroid of the polygons of allreefs
#' and assign it the values of the indicators. The search for the nearest centroid is performed over a circle of radius \code{max.radius}, expressed in meters.
#' The default value for \code{max.radius}, 5000 meters, reflects the size of the polygons of allreefs (0.05 degrees, which is approximately 5 km).
#' If no reef polygon centroid is found within a circle of radius \code{max.radius} centered on the input point, the funciton will return NA for that input point.
#' Increasing \code{max.radius} can extend the search to reef polygons located farther away.
#'
#' @examples
#' # Read data
#' data <- read.csv("coral-thresholds-lat-longs.csv")
#' # Open allreefs data
#' allreefs <- rgdal::readOGR(here::here("data"),"allreefs")
#' # Extract the values of the indicators
#' indicators <- extract_values(data, allreefs, max.radius = 20000)

extract_values <- function(data, allreefs, max.radius=5000) {

  # Read CRS for allreefs
  prj4 <- sp::proj4string(allreefs)

  cat("CRS for allreefs is\n",prj4,"\n")

  # Convert points to spatialPoints. Assuming input data are in WGS84 (EPSG: 4326)
  points <- sp::SpatialPoints(data[,c(3,4)],
                               proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))


  # Change CRS for points
  points_prj4 <- sp::spTransform(points,sp::CRS(prj4))

  # Extract allreefs centroids
  allreefs_centroids <- rgeos::gCentroid(allreefs, byid=T)

  # Find nearest neighboring allreefs for each point
  nn <- RANN::nn2(sp::coordinates(allreefs_centroids),
                  sp::coordinates(points_prj4),
                  k=1,
                  searchtype = "radius",
                  radius = max.radius)


  # Initializing output dataframe
# LEGEND:
# "score", Climate: composite score
# "scorecn", Climate: connectivity
# "scorecy", Climate: Cyclone Risk
# "scorepfc", Climate: Thermal future
# "scoreth", Climate: Thermal history
# "scoretr", Climate: Recent stress
# "grav_NC", Fishing: Market Pressure
# "sediment", Pollution: Sedimentation
# "nutrient", Pollution: Nutrients
# "pop_count", Coastal Development: Human Population
# "num_ports", Industrial Development: Ports
# "reef_value", Tourism: Reef Value
  out.data <- matrix(NA, nrow=nrow(nn$nn.idx), ncol = 12)
  colnames(out.data) <- c("score",
                          "scorecn",
                          "scorecy",
                          "scorepfc",
                          "scoreth",
                          "scoretr",
                          "grav_NC",
                          "sediment",
                          "nutrient",
                          "pop_count",
                          "num_ports",
                          "reef_value")
  out.data <- as.data.frame(out.data)

  # Loop on points to fill values into output dataframe
  for (i in 1 : nrow(nn$nn.idx)) {
    if (nn$nn.idx[i,1] == 0) next
    out.data[i, ] <- as.list(allreefs@data[nn$nn.idx[i,1],
                                           c("score",
                                             "scorecn",
                                             "scorecy",
                                             "scorepfc",
                                             "scoreth",
                                             "scoretr",
                                             "grav_NC",
                                             "sediment",
                                             "nutrient",
                                             "pop_count",
                                             "num_ports",
                                             "reef_value")])
  }
  out.data <- cbind(data,out.data)
  return(out.data)
}

