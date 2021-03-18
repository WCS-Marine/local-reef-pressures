# Function to loop the intersection operation over tiles
# Marco Andrello
# 26/03/2020
IntersectByTile <- function(reefcells, points, step=1e6) {
  # reefcells is a SpatialPolygonsDataFrame: the reef cells (or their buffers)
  # points is a SpatialPointsDataFrame: the points with the values (ex: population counts or reef value)
  #        The field of interest in points must be called "values"

  # reefcells <- allreefs_buffer
  # points <- p
  # step <- 1e6

  # Initialize output
  output <- rep(0, dim(reefcells)[1])
  # Define the bundaries of the tiles
  # we'll go from -20,000,000 to +20,000,000 (Easting) and from -4,000,000 to 4,000,000 (Northing)
  seq.easting <- seq(-20e6,20e6,step)
  seq.northing <- seq(-4e6,4e6,step)
  # Initialize tile
  tile <- bbox(reefcells)
  # Loop on tiles
  for (i.x in 1 : (length(seq.easting)-1)) {
    for (i.y in 1 : (length(seq.northing)-1)) {
      cat(i.x,i.y,"\n"); flush.console()
      tile[1,1] <- seq.easting[i.x]
      tile[1,2] <- seq.easting[i.x+1]
      tile[2,1] <- seq.northing[i.y]
      tile[2,2] <- seq.northing[i.y+1]
      tile_pol <- bbox_to_SpatialPolygons(tile,
                                          proj4string = CRS(proj4string(reefcells)))
      # Clip reefcells into tile
      reefcells_tile <- gPolyByIntersect2(tile_pol,
                                          reefcells,
                                          centroid=T)
      if (is.null(reefcells_tile)) next
      # Points contained in this tile
      points_tile <- gBoundingPoints(points,
                                     bbox(reefcells_tile))
      # Intersection
      is <- gIntersects(reefcells_tile,
                        points_tile,
                        byid = T)
      for (i.cell in 1 : dim(reefcells_tile)[1]) {
        cp <- which(is[, i.cell]) # points contained in this cell
        if (length(cp > 0)) {
          obj.id.cell <- reefcells_tile@data$OBJECTID[i.cell] # Unique identifier for the cell
          id <- match(obj.id.cell,
                      reefcells@data$OBJECTID)                # Matches the unique identifier of the global and tile dataframes
          output[id] <- sum(points_tile$values[cp])           # Calculates value and stores it in the good position in the global dataframe
        }
      }
    }
  }
  return(output)
}

