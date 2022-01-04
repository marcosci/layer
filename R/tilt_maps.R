#' Tilt raster and sf data
#'
#' Tilt and shift maps in any direction.#'
#'
#' @param data sf or terra/stars/raster object.
#' @param x_stretch Stretch in x dimension
#' @param y_stretch Stretch in y dimension
#' @param x_tilt Tilt in x dimension
#' @param y_tilt Tilt in y dimension
#' @param x_shift Shift in x dimension
#' @param y_shift Shift in y dimension
#'
#' @details
#' Code adopted from https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/geospatial-data/.
#'
#' @return sf
#' @export
#' @examples
#' tilt_map(landscape_1)
tilt_map <- function(data,
                     x_stretch = 2,
                     y_stretch = 1.2,
                     x_tilt = 0,
                     y_tilt = 1,
                     x_shift = 0,
                     y_shift = 0) {
  if (!any(class(data) %in% c("sf", "sfg"))) {
    data <- stars::st_as_stars(data)
    data <- sf::st_as_sf(data)
  }

  shear_matrix <- function(x) {
    matrix(c(x_stretch, y_stretch, x_tilt, y_tilt), 2, 2)
  }

  rotate_matrix <- function(x) {
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2)
  }

  data$geometry <- data$geometry * shear_matrix() * rotate_matrix(pi / 20) + c(x_shift, y_shift)
  
  if(length(names(data)) > 1) names(data)[1] <- "value"
  
  return(data)
  
}
