gPolyByIntersect2 <- function (container, reference, threshold = 0, centroid = F) {
# From mSpatial::gPolyByIntersect
# container <- tile_pol
# reference <- allreefs_buffer
# threshold = 0
# centroid = T

  if (centroid) {
    centroids = gCentroid(reference, byid = T)
    ta_blocks_bbox_centroids_idx = gWhichPoints(centroids,
                                                bbox(container))
    if (length(ta_blocks_bbox_centroids_idx) == 0) return(NULL)  ### Added this line to avoid error when the intersection is empty
    ta_blocks_bbox = reference[ta_blocks_bbox_centroids_idx, ]
    ta_blocks_bbox[which(gContains(container, gCentroid(ta_blocks_bbox,
                                                        byid = T), byid = T)), ]
  }
  else {
    slivers = raster::intersect(reference, container)
    sliver.area = gArea(spTransform(slivers, CRS("+init=epsg:3857")),
                        byid = T)
    reference.area = gArea(spTransform(reference[slivers,
                                                 ], CRS("+init=epsg:3857")), byid = T)
    overlap = sliver.area/reference.area
    reference[slivers, ][which(overlap > threshold), ]
  }
}

