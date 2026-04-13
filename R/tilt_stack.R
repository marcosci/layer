#' @importFrom memoise memoise
.tilt_map_mem <- memoise::memoise(tilt_map)

#' Initialize a tilt stack
#'
#' @param x_shift_step Default shift in x dimension per layer.
#' @param y_shift_step Default shift in y dimension per layer.
#' @param x_stretch Default stretch in x dimension.
#' @param y_stretch Default stretch in y dimension.
#' @param x_tilt Default tilt in x dimension.
#' @param y_tilt Default tilt in y dimension.
#' @param angle_rotate Default rotation angle.
#' @param label_x Default X coordinate for layer labels.
#' @param label_y Default Y coordinate for layer labels. If `NULL`, defaults to the layer's `y_shift`.
#' @param label_color Default color for layer labels.
#' @param label_size Default size for layer labels.
#' @return An object of class `tilt_stack`.
#' @export
#'
#' @examples
#' tilt_stack()
tilt_stack <- function(
  x_shift_step = 0, y_shift_step = 0,
  x_stretch = 2, y_stretch = 1.2, x_tilt = 0, y_tilt = 1, angle_rotate = pi/20,
  label_x = NULL, label_y = NULL, label_color = "grey40", label_size = 4
) {
  structure(list(
    layers = list(), 
    connectors = list(),
    defaults = list(
      x_shift_step = x_shift_step, y_shift_step = y_shift_step,
      x_stretch = x_stretch, y_stretch = y_stretch, 
      x_tilt = x_tilt, y_tilt = y_tilt, angle_rotate = angle_rotate,
      label_x = label_x, label_y = label_y, label_color = label_color, label_size = label_size
    )
  ), class = "tilt_stack")
}

#' Add a layer to a tilt stack
#'
#' @param stack A `tilt_stack` object.
#' @param data An `sf` or raster object.
#' @param x_shift Absolute shift in x dimension. If `NULL` and `x_shift_rel` is `NULL`, inherits from stack defaults.
#' @param y_shift Absolute shift in y dimension. If `NULL` and `y_shift_rel` is `NULL`, inherits from stack defaults.
#' @param x_shift_rel Relative shift in x dimension from the previous layer's position.
#' @param y_shift_rel Relative shift in y dimension from the previous layer's position.
#' @param x_stretch Stretch in x dimension. If `NULL`, inherits from stack defaults.
#' @param y_stretch Stretch in y dimension. If `NULL`, inherits from stack defaults.
#' @param x_tilt Tilt in x dimension. If `NULL`, inherits from stack defaults.
#' @param y_tilt Tilt in y dimension. If `NULL`, inherits from stack defaults.
#' @param angle_rotate Rotation angle. If `NULL`, inherits from stack defaults.
#' @param fill Column name to use for fill and color aesthetics. If `NA` (or if the column is not found in the data), a static color is used. Defaults to `"value"`.
#' @param color A single color string used if `fill` is `NA` or not found.
#' @param palette Palette name from the \link[viridis]{viridis} or \link[scico]{scico} package.
#' @param direction Direction for the color palette (1 or -1).
#' @param begin Start of interval for palette.
#' @param end End of interval for palette.
#' @param alpha Opacity of the layer.
#' @param size Size of the points or lines in the layer. If `NULL`, defaults to `0.01` for the first layer and `0.5` for others.
#' @param label Optional text label to annotate the layer.
#' @param label_x X coordinate for the label. If `NULL`, inherits from stack defaults.
#' @param label_y Y coordinate for the label. If `NULL`, inherits from stack defaults or `y_shift`.
#' @param label_color Color for the label. If `NULL`, inherits from stack defaults.
#' @param label_size Size for the label. If `NULL`, inherits from stack defaults.
#'
#' @return The updated `tilt_stack`.
#' @export
tilt_layer <- function(
  stack, data,
  x_shift = NULL, y_shift = NULL,
  x_shift_rel = NULL, y_shift_rel = NULL,
  x_stretch = NULL, y_stretch = NULL, x_tilt = NULL, y_tilt = NULL, angle_rotate = NULL,
  fill = "value", color = "grey50", palette = "viridis", direction = 1, begin = 0, end = 1, alpha = 1,
  size = NULL,
  label = NA, label_x = NULL, label_y = NULL, label_color = NULL, label_size = NULL
) {
  layer_def <- list(
    data = data,
    x_shift = x_shift, y_shift = y_shift,
    x_shift_rel = x_shift_rel, y_shift_rel = y_shift_rel,
    x_stretch = x_stretch, y_stretch = y_stretch, x_tilt = x_tilt, y_tilt = y_tilt, angle_rotate = angle_rotate,
    fill = fill, color = color, palette = palette, direction = direction, begin = begin, end = end, alpha = alpha,
    size = size,
    label = label, label_x = label_x, label_y = label_y, label_color = label_color, label_size = label_size
  )
  stack$layers[[length(stack$layers) + 1]] <- layer_def
  stack
}
#' Add connector lines to a tilt stack
#'
#' @param stack A `tilt_stack` object.
#' @param data An `sf` object representing the points/polygons to connect.
#' @param color Color of the connector lines.
#' @param alpha Opacity of the connector lines.
#' @param size Size/thickness of the connector lines. Default is `1.0`.
#' @param draw_points Logical; if `TRUE`, draws the points on the top layer.
#' @param point_color Color of the points drawn on top.
#' @param point_size Size of the points drawn on top.
#' @param on_top Logical; if `TRUE`, connector lines are drawn on top of all layers. If `FALSE` (default), lines are interleaved between layers for perfect 3D occlusion.
#'
#' @return The updated `tilt_stack`.
#' @export
tilt_connector <- function(
  stack, data,
  color = "grey60", alpha = 1, size = 1.0,
  draw_points = TRUE, point_color = "black", point_size = 1,
  on_top = FALSE
) {
  conn_def <- list(
    data = data,
    color = color, alpha = alpha, size = size,
    draw_points = draw_points, point_color = point_color, point_size = point_size,
    on_top = on_top,
    pos = length(stack$layers) # Record position in pipeline
  )
  stack$connectors[[length(stack$connectors) + 1]] <- conn_def
  stack
}

#' @noRd
.resolve_params <- function(stack) {
  resolved <- list()
  curr_x <- 0
  curr_y <- 0
  
  for (i in seq_along(stack$layers)) {
    l <- stack$layers[[i]]
    
    # Shifts
    xs <- if (!is.null(l$x_shift)) l$x_shift else if (!is.null(l$x_shift_rel)) curr_x + l$x_shift_rel else if (i == 1) 0 else curr_x + stack$defaults$x_shift_step
    ys <- if (!is.null(l$y_shift)) l$y_shift else if (!is.null(l$y_shift_rel)) curr_y + l$y_shift_rel else if (i == 1) 0 else curr_y + stack$defaults$y_shift_step
    
    # Stretches
    xst <- if (!is.null(l$x_stretch)) l$x_stretch else stack$defaults$x_stretch
    yst <- if (!is.null(l$y_stretch)) l$y_stretch else stack$defaults$y_stretch
    
    # Tilts
    xt <- if (!is.null(l$x_tilt)) l$x_tilt else stack$defaults$x_tilt
    yt <- if (!is.null(l$y_tilt)) l$y_tilt else stack$defaults$y_tilt
    
    # Rotate
    rot <- if (!is.null(l$angle_rotate)) l$angle_rotate else stack$defaults$angle_rotate
    
    resolved[[i]] <- list(
      x_shift = xs, y_shift = ys,
      x_stretch = xst, y_stretch = yst,
      x_tilt = xt, y_tilt = yt,
      angle_rotate = rot
    )
    
    curr_x <- xs
    curr_y <- ys
  }
  resolved
}

#' Plot a tilt stack
#'
#' @param stack A `tilt_stack` object.
#'
#' @return A `ggplot` object.
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \donttest{
#' stack <- tilt_stack(x_shift_step = 25, y_shift_step = 50)
#' stack <- tilt_layer(stack, landscape_1, palette = "bilbao")
#' stack <- tilt_layer(stack, landscape_2, palette = "mako")
#' stack <- tilt_connector(stack, landscape_points)
#' plot_tilt_stack(stack)
#' }
plot_tilt_stack <- function(stack) {
  if (length(stack$layers) == 0) return(ggplot2::ggplot() + ggplot2::theme_void())

  # 1. Resolve all spatial parameters once for the whole stack
  params <- .resolve_params(stack)

  # 2. Pre-calculate global xmin for label positioning (use bbox corners for speed)
  global_xmin <- NULL
  if (is.null(stack$defaults$label_x)) {
    xmins <- sapply(seq_along(stack$layers), function(i) {
      l <- stack$layers[[i]]
      p <- params[[i]]
      bb <- sf::st_bbox(l$data)
      corners <- matrix(c(bb[1], bb[2], bb[3], bb[2], bb[3], bb[4], bb[1], bb[4]), ncol = 2, byrow = TRUE)
      sm <- matrix(c(p$x_stretch, p$y_stretch, p$x_tilt, p$y_tilt), 2, 2)
      rm <- matrix(c(cos(p$angle_rotate), sin(p$angle_rotate), -sin(p$angle_rotate), cos(p$angle_rotate)), 2, 2)
      t_corners <- corners %*% sm %*% rm + rep(c(p$x_shift, p$y_shift), each = 4)
      min(t_corners[, 1])
    })
    global_xmin <- min(xmins)
  }

  p <- ggplot2::ggplot()
  has_mapped_fill <- FALSE
  has_labels <- FALSE
  connector_layers <- list()

  # 3. Main Rendering Loop
  for (i in seq_along(stack$layers)) {
    layer_def <- stack$layers[[i]]
    p_curr <- params[[i]]
    
    # Use memoised tilt function
    tilted_data <- .tilt_map_mem(layer_def$data, x_stretch = p_curr$x_stretch, y_stretch = p_curr$y_stretch, x_tilt = p_curr$x_tilt, y_tilt = p_curr$y_tilt, x_shift = p_curr$x_shift, y_shift = p_curr$y_shift, angle_rotate = p_curr$angle_rotate)

    # Fallback if fill column is missing or non-numeric
    if (!is.na(layer_def$fill)) {
      if (!(layer_def$fill %in% names(tilted_data)) || !is.numeric(tilted_data[[layer_def$fill]])) {
        layer_def$fill <- NA
      }
    }

    # Draw Map Layer
    if (!is.na(layer_def$fill)) {
      if (has_mapped_fill) {
        p <- p + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
      }
      layer_size <- if (!is.null(layer_def$size)) layer_def$size else (if (i == 1) 0.01 else 0.5)
      p <- p + ggplot2::geom_sf(data = tilted_data, ggplot2::aes(fill = .data[[layer_def$fill]], color = .data[[layer_def$fill]]), size = layer_size, alpha = layer_def$alpha)
      has_mapped_fill <- TRUE

      # Apply scales
      if (layer_def$palette %in% c("viridis", "inferno", "magma", "plasma", "cividis", "mako", "rocket", "turbo", letters[1:9])) {
        p <- p + ggplot2::scale_fill_viridis_c(option = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none") +
                 ggplot2::scale_color_viridis_c(option = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none")
      } else if (layer_def$palette %in% scico::scico_palette_names()) {
        p <- p + scico::scale_fill_scico(palette = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none") +
                 scico::scale_color_scico(palette = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none")
      }
    } else {
      layer_size <- if (!is.null(layer_def$size)) layer_def$size else (if (i == 1) 0.01 else 0.5)
      p <- p + ggplot2::geom_sf(data = tilted_data, color = layer_def$color, alpha = layer_def$alpha, size = layer_size)
    }

    # 4. Collect Connectors
    if (i < length(stack$layers) && length(stack$connectors) > 0) {
      p_next <- params[[i + 1]]
      for (j in seq_along(stack$connectors)) {
        conn <- stack$connectors[[j]]
        is_global <- conn$pos == 0 || conn$pos == length(stack$layers)
        is_local_to_this_gap <- conn$pos == i
        
        if (is_global || is_local_to_this_gap) {
          # Use memoised tilt function for connectors too
          pts_curr <- .tilt_map_mem(conn$data, x_stretch = p_curr$x_stretch, y_stretch = p_curr$y_stretch, x_tilt = p_curr$x_tilt, y_tilt = p_curr$y_tilt, x_shift = p_curr$x_shift, y_shift = p_curr$y_shift, angle_rotate = p_curr$angle_rotate)
          pts_next <- .tilt_map_mem(conn$data, x_stretch = p_next$x_stretch, y_stretch = p_next$y_stretch, x_tilt = p_next$x_tilt, y_tilt = p_next$y_tilt, x_shift = p_next$x_shift, y_shift = p_next$y_shift, angle_rotate = p_next$angle_rotate)
          
          lines_geom <- tilt_lines(pts_curr, pts_next)
          
          if (conn$on_top) {
            # Defer to end
            connector_layers[[length(connector_layers) + 1]] <- ggplot2::geom_sf(data = lines_geom, color = conn$color, alpha = conn$alpha, size = conn$size)
          } else {
            # Draw now (so next map layer can occlude it)
            p <- p + ggplot2::geom_sf(data = lines_geom, color = conn$color, alpha = conn$alpha, size = conn$size)
          }
        }
      }
    }

    # 5. Annotations
    if (!is.na(layer_def$label)) {
      lbl_x <- if (!is.null(layer_def$label_x)) layer_def$label_x else if (!is.null(stack$defaults$label_x)) stack$defaults$label_x else global_xmin - 5
      lbl_y <- if (!is.null(layer_def$label_y)) layer_def$label_y else p_curr$y_shift
      lbl_col <- if (!is.null(layer_def$label_color)) layer_def$label_color else stack$defaults$label_color
      lbl_size <- if (!is.null(layer_def$label_size)) layer_def$label_size else stack$defaults$label_size
      lbl_hjust <- if (is.null(layer_def$label_x) && is.null(stack$defaults$label_x)) 1 else 0

      if (!is.null(lbl_x) && !is.null(lbl_y)) {
        p <- p + ggplot2::annotate("text", label = layer_def$label, x = lbl_x, y = lbl_y, color = lbl_col, size = lbl_size, hjust = lbl_hjust)
        has_labels <- TRUE
      }
    }
  }

  # 6. Draw all connector lines on top
  for (cl in connector_layers) {
    p <- p + cl
  }

  # 7. Draw Top Points
  if (length(stack$connectors) > 0) {
    p_top <- params[[length(params)]]
    for (j in seq_along(stack$connectors)) {
      conn <- stack$connectors[[j]]
      is_global <- conn$pos == 0 || conn$pos == length(stack$layers)
      is_top_layer_connector <- conn$pos == (length(stack$layers) - 1)
      
      if (conn$draw_points && (is_global || is_top_layer_connector)) {
        # Use memoised tilt function
        pts_top <- .tilt_map_mem(conn$data, x_stretch = p_top$x_stretch, y_stretch = p_top$y_stretch, x_tilt = p_top$x_tilt, y_tilt = p_top$y_tilt, x_shift = p_top$x_shift, y_shift = p_top$y_shift, angle_rotate = p_top$angle_rotate)
        
        geom_types <- as.character(sf::st_geometry_type(pts_top))
        if (any(geom_types != "POINT")) {
          pts_top <- suppressWarnings(sf::st_centroid(sf::st_geometry(pts_top)))
          pts_top <- sf::st_as_sf(pts_top)
        }
        p <- p + ggplot2::geom_sf(data = pts_top, color = conn$point_color, size = conn$point_size)
      }
    }
  }

  p <- p + ggplot2::theme_void()
  if (has_labels) {
    p <- p + 
      ggplot2::coord_sf(clip = "off") +
      ggplot2::theme(plot.margin = ggplot2::margin(l = 100)) # Add margin to prevent label truncation
  }
  p
}

#' Print a tilt stack
#'
#' @param x A `tilt_stack` object.
#' @param ... Additional arguments passed to `print`.
#' @return The original `tilt_stack` object, invisibly.
#' @export
#' @method print tilt_stack
print.tilt_stack <- function(x, ...) {
  print(plot_tilt_stack(x))
  invisible(x)
}
