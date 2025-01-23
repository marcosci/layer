#' Tilt raster and sf data
#'
#' Takes tilted maps and plots them with ggplot.
#'
#' @param map_list sf or terra/stars/raster object.
#' @param layer vector or list of names of each column in tilted sf object that should be used for coloring
#' @param palette vector of palettes provided by the \link[viridis]{viridis} and \link[scico]{scico} packages for rasters
#' @param color  a single color applied multiple times or a vector of color strings for points or linestrings
#' @param direction vector of directions for \link[viridis]{viridis} and \link[scico]{scico} color palettes
#' @param begin vector of the of the start of interval the palette to sample colours from for \link[viridis]{viridis} and \link[scico]{scico} color palettes
#' @param end vector of the of the end of interval the palette to sample colours from for \link[viridis]{viridis} and \link[scico]{scico} color palettes
#' @param alpha vector of opacity for \link[viridis]{viridis} and \link[scico]{scico} color palettes
#' 
#'
#' @return ggplot
#' @importFrom sf st_geometry_type
#' @export
#' @examples
#' # tilt data
#' tilt_landscape_1 <- tilt_map(landscape_1)
#' tilt_landscape_2 <- tilt_map(landscape_2, x_shift = 50, y_shift = 50)
#'
#' # plot
#' map_list <- list(tilt_landscape_1, tilt_landscape_2)
#' plot_tiltedmaps(map_list, palette = "turbo")

plot_tiltedmaps <- function(map_list, layer = NA, palette = "viridis", color = "grey50", direction = 1, begin = 0, end = 1, alpha = 1) {
  
  ## checks ----
  if(all(is.na(layer))) layer <- "value"
  if(length(layer) == 1) layer <- rep(layer, length(map_list))
  if(length(alpha) == 1) alpha <- rep(alpha, length(map_list))
  if(length(direction) == 1) direction <- rep(direction, length(map_list))
  if(length(begin) == 1) begin <- rep(begin, length(map_list))
  if(length(end) == 1) end <- rep(end, length(map_list))
  
  # fill in palettes and colors
  if(length(palette) == 1) palette <- rep(palette, length(map_list))
  if(length(color) == 1) color <- rep(color, length(map_list))
  
  #if(!palette %in% c("viridis", "inferno", "magma", "plasma", "cividis", "mako", "rocket", "turbo", letters[1:9], scico::scico_palette_names())) stop("palette should be a palette name from the \link[viridis]{viridis} or \link[scico]{scico} package.")
  
  ## plot ----
  map_tilt <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = map_list[[1]],
      ggplot2::aes_string(fill = layer[[1]],
                 color = layer[[1]]), size = 0.01
    ) +
    {
      if (palette[1] %in% c("viridis", "inferno", "magma", "plasma", "cividis", "mako", "rocket", "turbo", letters[1:9])) 
        ggplot2::scale_fill_viridis_c(option = palette[1], direction = direction[1], begin = begin[1], end = end[1], alpha = alpha[1], guide = "none")
    } +
    {
      if (palette[1] %in% c("viridis", "inferno", "magma", "plasma", "cividis", "mako", "rocket", "turbo", letters[1:9])) 
        ggplot2::scale_color_viridis_c(option = palette[1], direction = direction[1], begin = begin[1], end = end[1], alpha = alpha[1], guide = "none")
    } +
    {
      if (palette[1] %in% scico::scico_palette_names()) 
        scico::scale_fill_scico(palette = palette[1], direction = direction[1], begin = begin[1], end = end[1], alpha = alpha[1], guide = "none")
    } +
    {
      if (palette[1] %in% scico::scico_palette_names()) 
        scico::scale_color_scico(palette = palette[1], direction = direction[1], begin = begin[1], end = end[1], alpha = alpha[1], guide = "none")
    }
  
  
  
  if(length(map_list) > 1) {
    for (i in seq_along(map_list)[-1]) {
      if(!is.na(layer[[i]])){
        map_tilt <- map_tilt +
          ggnewscale::new_scale_fill() +
          ggnewscale::new_scale_color()  +
          ggplot2::geom_sf(
            data = map_list[[i]],
            ggplot2::aes_string(fill = layer[[i]],
                       color = layer[[i]]), size = .5
          ) +
          {
            if (palette[i] %in% c("viridis", "inferno", "magma", "plasma", "cividis", "mako", "rocket", "turbo", letters[1:9])) 
              ggplot2::scale_fill_viridis_c(option = palette[i], direction = direction[i], begin = begin[i], end = end[i], alpha = alpha[i], guide = "none")
          } +
          {
            if (palette[i] %in% c("viridis", "inferno", "magma", "plasma", "cividis", "mako", "rocket", "turbo", letters[1:9])) 
              ggplot2::scale_color_viridis_c(option = palette[i], direction = direction[i], begin = begin[i], end = end[i], alpha = alpha[i], guide = "none")
          } +
          {
            if (palette[i] %in% scico::scico_palette_names()) 
              scico::scale_fill_scico(palette = palette[i], direction = direction[i], begin = begin[i], end = end[i], alpha = alpha[i], guide = "none")
          } +
          {
            if (palette[i] %in% scico::scico_palette_names()) 
              scico::scale_color_scico(palette = palette[i], direction = direction[i], begin = begin[i], end = end[i], alpha = alpha[i], guide = "none")
          }     
      } else {
        map_tilt <- map_tilt +
          ggplot2::geom_sf(
            data  = map_list[[i]],
            color = color[i],
            alpha = alpha[i]
          )
      }
    } 
  }
  
  map_tilt +
    ggplot2::theme_void() 

}
