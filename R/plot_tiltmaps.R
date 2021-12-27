#' Tilt raster and sf data
#'
#' Takes tilted maps and plots them with ggplot.
#'
#' @param map_list sf or terra/stars/raster object.
#' @param layer vector or list of names of each column in tilted sf object that should be used for coloring
#' @param palette vector of palettes provided by the {viridis} and {scico} packages for rasters or a single color for points or linestrings
#' @param direction vector of directions for {viridis} and {scico} color palettes
#' @param begin vector of the of the start of interval the palette to sample colours from for {viridis} and {scico} color palettes
#' @param end vector of the of the end of interval the palette to sample colours from for {viridis} and {scico} color palettes
#' @param alpha vector of opacity for {viridis} and {scico} color palettes
#' 
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

plot_tiltedmaps <- function(map_list, layer, palette = "viridis", direction = 1, begin = 0, end = 1, alpha = 1) {
  
  #if(!palette %in% c("viridis", "inferno", "magma", "plasma", "cividis", "mako", "rocket", "turbo", letters[1:9], scico::scico_palette_names())) stop("palette should be a palette name from the {viridis} or {scico} package.")
  
  if(length(layer) == 1) layer <- rep(layer, length(map_list))
  map_tilt <- ggplot() +
    geom_sf(
      data = map_list[[1]],
      aes_string(fill = layer[[1]],
                 color = layer[[1]]), size = 0.01
    )
  
  for (i in seq_along(map_list)) {
    if(!is.na(layer[[i]])){
      map_tilt <- map_tilt +
        geom_sf(
          data = map_list[[i]],
          aes_string(fill = layer[[i]],
                     color = layer[[i]]), size = .5
        ) +
        {
          if (palette[i] %in% c("viridis", "inferno", "magma", "plasma", "cividis", "mako", "rocket", "turbo", letters[1:9])) 
            scale_fill_viridis_c(option = palette[i], direction = direction, begin = begin, end = end, alpha = alpha, guide = "none")
        } +
        {
          if (palette[i] %in% c("viridis", "inferno", "magma", "plasma", "cividis", "mako", "rocket", "turbo", letters[1:9])) 
            scale_color_viridis_c(option = palette[i], direction = direction, begin = begin, end = end, alpha = alpha, guide = "none")
        } +
        {
          if (palette[i] %in% scico::scico_palette_names()) 
            scico::scale_fill_scico(palette = palette[i], direction = direction, begin = begin, end = end, alpha = alpha, guide = "none")
        } +
        {
          if (palette[i] %in% scico::scico_palette_names()) 
            scico::scale_color_scico(palette = palette[i], direction = direction, begin = begin, end = end, alpha = alpha, guide = "none")
        } +
        ggnewscale::new_scale_fill() +
        ggnewscale::new_scale_color()      
    } else {
      map_tilt <- map_tilt +
        geom_sf(
          data = map_list[[i]],
          color = palette[i],
          alpha = alpha[i]
        )
    }
  }
  
  map_tilt +
    theme_void() 
}
