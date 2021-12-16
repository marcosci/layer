#' Tilt raster and sf data 
#'
#' Takes tilted maps and plots them with ggplot.
#' 
#' @param data sf or terra/stars/raster object.
#' 
#' @return ggplot 
#' @export
#' @examples
#' # tilt data
#' tilt_landscape_1 <- tilt_map(landscape_1)
#' tilt_landscape_2 <- tilt_map(landscape_2, x_shift = 50, y_shift = 50)
#' 
#' # put in list
#' map_list <- list(tilt_landscape_1, tilt_landscape_2)
#' 
#' # plot
#' plot_tiltedmaps(map_list)

plot_tiltedmaps <- function(map_list){
  
  map_tilt <- ggplot() +
    geom_sf(data = map_list[[1]],
            aes(fill = layer), color = NA)
  
  for(i in seq_along(map_list)){
    map_tilt <- map_tilt + 
      geom_sf(data = map_list[[i]],
              aes(fill = layer), color = NA)
  }
  
  map_tilt +
    scale_fill_scico(palette = 'davos',
                     guide = 'none') +
    theme(legend.position = "none") +
    theme_void()
  
}