#' Create lines connecting corresponding features between two tilted maps
#'
#' This function creates `LINESTRING` geometries connecting the features of two
#' identically-structured `sf` objects. It is designed to connect points or polygon
#' centroids between layers that have been shifted differently using `tilt_map()`.
#'
#' @param data1 An `sf` object (e.g., points or polygons).
#' @param data2 An `sf` object with the same number of rows as `data1`.
#'
#' @return An `sf` object containing `LINESTRING` geometries connecting the features
#' of `data1` and `data2`. Non-spatial attributes from `data1` are preserved.
#' @importFrom sf st_geometry_type st_centroid st_geometry st_sfc st_linestring st_as_sf st_crs
#' @importFrom rlang abort
#' @export
#'
#' @examples
#' \donttest{
#' # create points
#' pts <- data.frame(x = c(1, 2), y = c(1, 2), id = c(1, 2))
#' pts <- sf::st_as_sf(pts, coords = c("x", "y"))
#'
#' # tilt and shift
#' pts_tilt1 <- tilt_map(pts)
#' pts_tilt2 <- tilt_map(pts, x_shift = 10, y_shift = 10)
#'
#' # connect
#' lines <- tilt_lines(pts_tilt1, pts_tilt2)
#'
#' # plot
#' plot_tiltedmaps(list(pts_tilt1, lines, pts_tilt2))
#' }
tilt_lines <- function(data1, data2) {
  if (!inherits(data1, "sf") || !inherits(data2, "sf")) {
    rlang::abort("Both `data1` and `data2` must be `sf` objects.")
  }
  
  if (nrow(data1) != nrow(data2)) {
    rlang::abort("`data1` and `data2` must have the same number of rows.")
  }
  
  # Helper to get centroids if polygons or mixed types, or points otherwise
  get_points <- function(x) {
    geom_types <- as.character(sf::st_geometry_type(x))
    if (any(geom_types != "POINT")) {
      suppressWarnings(sf::st_centroid(sf::st_geometry(x)))
    } else {
      sf::st_geometry(x)
    }
  }
  
  pts1 <- get_points(data1)
  pts2 <- get_points(data2)
  
  # Create lines
  lines_list <- lapply(seq_len(nrow(data1)), function(i) {
    p1 <- as.numeric(pts1[[i]])[1:2]
    p2 <- as.numeric(pts2[[i]])[1:2]
    sf::st_linestring(matrix(c(p1[1], p1[2], p2[1], p2[2]), ncol = 2, byrow = TRUE))
  })
  
  lines_sfc <- sf::st_sfc(lines_list, crs = sf::st_crs(data1))
  
  # Keep attributes from data1, replace geometry
  res <- data1
  sf::st_geometry(res) <- lines_sfc
  
  res
}
