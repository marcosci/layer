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
#' @param boundary Another layer that is used to create a boundary that is drawn around the data
#' @param parallel \code{logical} to run in parallel. FALSE (default)
#'
#' @details
#' Code adopted from https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/geospatial-data/.
#'
#' @return sf
#' @importFrom magrittr "%>%"
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
                     boundary = NULL,
                     parallel = FALSE) {
  
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

  if(!is.null(boundary)) data <- create_outline(boundary, data)
  
  if(parallel == TRUE){
    
  geom_func <- function(data, x_stretch, y_stretch, x_tilt, y_tilt, x_shift, y_shift){
    sf::st_geometry(data) <- sf::st_geometry(data) * shear_matrix() * rotate_matrix(pi / 20) + c(x_shift, y_shift) 
    data <- data %>% sf::st_as_sf()
    }
    
  data <- data %>%
    dplyr::group_by(group = (dplyr::row_number()-1) %/% (dplyr::n()/10))%>%
    tidyr::nest() %>% 
    dplyr::pull(data) %>%
    furrr::future_map(~geom_func(data = .,
                                          x_stretch = x_stretch,
                                          y_stretch = y_stretch,
                                          x_tilt = x_tilt,
                                          y_tilt = y_tilt,
                                          x_shift = x_shift,
                                          y_shift = y_shift)) %>% 
    dplyr::bind_rows() %>% 
    sf::st_as_sf()
  
    } else {
    
    sf::st_geometry(data) <- sf::st_geometry(data) * shear_matrix() * rotate_matrix(pi / 20) + c(x_shift, y_shift)
  
    }
  
  if(length(names(data)) > 1) names(data)[1] <- "value"
  
  return(data)
  
}

create_outline <- function(outline_from, outline_to){
  
  if (!any(class(outline_from) %in% c("sf", "sfg"))) {
    outline_from <- stars::st_as_stars(outline_from)
    outline_from <- sf::st_as_sf(outline_from)
  }
  
  outline_shape <- sf::st_union(outline_from) |> sf::st_cast('LINESTRING')
  
  rbind(
    outline_to,
    outline_shape
  )
  
}