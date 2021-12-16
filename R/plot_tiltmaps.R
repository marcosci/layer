#' Tilt raster and sf data
#'
#' Takes tilted maps and plots them with ggplot.
#'
#' @param map_list sf or terra/stars/raster object.
#' @param layer vector or list of names of each column in tilted sf object that should be used for coloring
#'
#' @return ggplot
#' @importFrom ggplot2 ggplot geom_sf aes theme theme_void
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
#' plot_tiltedmaps(map_list, "layer")

plot_tiltedmaps <- function(map_list, layer) {
  
  if(length(layer) == 1) layer <- rep(layer, length(map_list))
  
  map_tilt <- ggplot() +
    geom_sf(
      data = map_list[[1]],
      aes_string(fill = layer[[1]]), color = NA
    )

  for (i in seq_along(map_list)) {
    if(!is.na(layer[[i]])){
      map_tilt <- map_tilt +
        geom_sf(
          data = map_list[[i]],
          aes_string(fill = layer[[i]]), color = NA
        )
    } else {
      map_tilt <- map_tilt +
        geom_sf(
          data = map_list[[i]],
          color = "black"
        )
    }
  }

  map_tilt +
    scico::scale_fill_scico(
      palette = "davos",
      guide = "none"
    ) +
    theme(legend.position = "none") +
    theme_void()
}
