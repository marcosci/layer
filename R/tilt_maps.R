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
                     angle_rotate = pi/20,
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
    sf::st_geometry(data) <- sf::st_geometry(data) * shear_matrix() * rotate_matrix(angle_rotate) + c(x_shift, y_shift) 
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
    
    sf::st_geometry(data) <- sf::st_geometry(data) * shear_matrix() * rotate_matrix(angle_rotate) + c(x_shift, y_shift)
  
    }
  
  if(length(names(data)) > 1) names(data)[1] <- "value"
  
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
  
  current = attr(outline_shape, "sf_column")
  names(outline_shape)[names(outline_shape)==current] = "geometry"
  sf::st_geometry(outline_shape) = "geometry"
  
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