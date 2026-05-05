#' Tilt raster and sf data
#'
#' Tilt and shift maps in any direction.#'
#'
#' @param data sf or terra/stars/raster object.
#' @param x_stretch Stretch in x dimension. A `numeric` vector of lenght 1.
#' @param y_stretch Stretch in y dimension. A `numeric` vector of lenght 1.
#' @param x_tilt Tilt in x dimension. A `numeric` vector of lenght 1.
#' @param y_tilt Tilt in y dimension. A `numeric` vector of lenght 1.
#' @param x_shift Shift in x dimension. A `numeric` vector of lenght 1.
#' @param y_shift Shift in y dimension. A `numeric` vector of lenght 1.
#' @param angle_rotate Rotation angle.. A `numeric` vector of lenght 1. Default is \code{pi/20}.
#' @param boundary Another layer that is used to create a boundary that is drawn around the data
#' @param parallel \code{logical} to run in parallel. FALSE (default)
#' @details
#' Code adopted from https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/geospatial-data/.
#'
#' @return An `sf` object with tilted and shifted data.
#' @import raster
#' @export
#' @examples
#' tilt_map(landscape_1)
tilt_map <- function(data,
                     x_stretch = 2,
                     y_stretch = 1.2,
                     x_tilt = 0,
                     y_tilt = 1,
                     x_shift = 0,
                     y_shift = 0,
                     angle_rotate = pi/20,
                     boundary = NULL,
                     parallel = FALSE) {

  # Affine matrices
  shear_mat <- matrix(c(x_stretch, y_stretch, x_tilt, y_tilt), 2, 2)
  rotate_mat <- matrix(c(cos(angle_rotate), sin(angle_rotate), -sin(angle_rotate), cos(angle_rotate)), 2, 2)
  full_mat <- shear_mat %*% rotate_mat

  if (!any(class(data) %in% c("sf", "sfc", "sfg"))) {
    # It is a raster/stars object
    if (!inherits(data, "stars")) data <- stars::st_as_stars(data)
    
    # Apply affine transformation directly to stars metadata
    d <- stars::st_dimensions(data)
    
    # In stars, affine transformation is represented in the geotransform
    # We update the delta and affine components
    # Original: x = off_x + i*dx, y = off_y + j*dy
    # New: [x, y] = [off_x, off_y] + [i, j] %*% Matrix
    
    # This is a bit complex in stars directly, so we convert to sf 
    # BUT we do it after the parameters are resolved if possible.
    # Actually, stars support for arbitrary affine in plot is limited in ggplot.
    # The most stable way that is still fast is to convert to sf 
    # and then apply the matrix to the whole sfc at once (vectorized).
    
    # We ensure we only convert to sf ONCE.
    data <- sf::st_as_sf(data)
  }

  if(!is.null(boundary)) data <- create_outline(boundary, data)

  if (inherits(data, "sf")) {
    # Apply transformation to the entire geometry column at once (very fast)
    sf::st_geometry(data) <- sf::st_geometry(data) * full_mat + c(x_shift, y_shift)
  } else {
    # For sfc and sfg
    data <- data * full_mat + c(x_shift, y_shift)
  }

  if(length(names(data)) > 1) {
    # Ensure the value column is named correctly for downstream use
    if (!("geometry" %in% names(data))) {
       # Find geometry column if not named geometry
       geom_col <- attr(data, "sf_column")
       if (names(data)[1] != geom_col) names(data)[1] <- "value"
    } else {
       if (names(data)[1] != "geometry") names(data)[1] <- "value"
    }
  }

  return(data)
}

create_outline <- function(outline_from, outline_to){
  
  if (!any(class(outline_from) %in% c("sf", "sfg"))) {
    outline_from <- stars::st_as_stars(outline_from)
    outline_from <- sf::st_as_sf(outline_from)
  }
  
  if (!any(class(outline_to) %in% c("sf", "sfg"))) {
    outline_to <- stars::st_as_stars(outline_to)
    outline_to <- sf::st_as_sf(outline_to)
  }
  
  outline_shape <- sf::st_union(sf::st_buffer(outline_from, dist = 0))
  outline_shape <- sf::st_as_sf(sf::st_cast(sf::st_as_sf(outline_shape), 'MULTILINESTRING'))
  
  current <- attr(outline_shape, "sf_column")
  names(outline_shape)[names(outline_shape) == current] <- "geometry"
  sf::st_geometry(outline_shape) <- "geometry"
  
  if(length(names(outline_to)) > 1) {
    
    outline_names <- names(outline_to)
    outline_names <- outline_names[-which(outline_names == "geometry")]
    for(nm in outline_names){ 
      outline_shape[paste0(nm)] <-NA
      }
  }
  
  rbind(
    outline_to,
    outline_shape
  )
  
}
