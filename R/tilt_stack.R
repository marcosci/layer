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
#' @param x_shift Shift in x dimension. If `NULL`, inherits from stack defaults.
#' @param y_shift Shift in y dimension. If `NULL`, inherits from stack defaults.
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
  x_stretch = NULL, y_stretch = NULL, x_tilt = NULL, y_tilt = NULL, angle_rotate = NULL,
  fill = "value", color = "grey50", palette = "viridis", direction = 1, begin = 0, end = 1, alpha = 1,
  size = NULL,
  label = NA, label_x = NULL, label_y = NULL, label_color = NULL, label_size = NULL
) {
  layer_def <- list(
    data = data,
    x_shift = x_shift, y_shift = y_shift,
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
#' @param size Size/thickness of the connector lines.
#' @param draw_points Logical; if `TRUE`, draws the points on the top layer.
#' @param point_color Color of the points drawn on top.
#' @param point_size Size of the points drawn on top.
#'
#' @return The updated `tilt_stack`.
#' @export
tilt_connector <- function(
  stack, data,
  color = "grey60", alpha = 1, size = 0.5,
  draw_points = TRUE, point_color = "black", point_size = 1
) {
  conn_def <- list(
    data = data,
    color = color, alpha = alpha, size = size,
    draw_points = draw_points, point_color = point_color, point_size = point_size,
    pos = length(stack$layers) # Record position in pipeline
  )
  stack$connectors[[length(stack$connectors) + 1]] <- conn_def
  stack
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

  # Pre-calculate global xmin to place labels safely if label_x is not provided
  global_xmin <- NULL
  if (is.null(stack$defaults$label_x)) {
    xmins <- sapply(seq_along(stack$layers), function(i) {
      l <- stack$layers[[i]]
      # Resolve parameters for this layer
      xs <- if (!is.null(l$x_shift)) l$x_shift else (i - 1) * stack$defaults$x_shift_step
      xt <- if (!is.null(l$x_tilt)) l$x_tilt else stack$defaults$x_tilt
      xst <- if (!is.null(l$x_stretch)) l$x_stretch else stack$defaults$x_stretch
      yr <- if (!is.null(l$angle_rotate)) l$angle_rotate else stack$defaults$angle_rotate
      
      # Just get the bbox corners and transform them (much faster than transforming whole dataset)
      bb <- sf::st_bbox(l$data)
      corners <- matrix(c(bb[1], bb[2], bb[3], bb[2], bb[3], bb[4], bb[1], bb[4]), ncol = 2, byrow = TRUE)
      
      # Replicate tilt_map transform logic: corners * shear * rotate + shift
      sm <- matrix(c(xst, stack$defaults$y_stretch, xt, stack$defaults$y_tilt), 2, 2)
      rm <- matrix(c(cos(yr), sin(yr), -sin(yr), cos(yr)), 2, 2)
      
      t_corners <- corners %*% sm %*% rm + rep(c(xs, (i - 1) * stack$defaults$y_shift_step), each = 4)
      min(t_corners[, 1])
    })
    global_xmin <- min(xmins)
  }

  p <- ggplot2::ggplot()

  has_mapped_fill <- FALSE
  has_labels <- FALSE

  for (i in seq_along(stack$layers)) {
    layer_def <- stack$layers[[i]]

    # Resolve parameters
    x_shift <- if (!is.null(layer_def$x_shift)) layer_def$x_shift else (i - 1) * stack$defaults$x_shift_step
    y_shift <- if (!is.null(layer_def$y_shift)) layer_def$y_shift else (i - 1) * stack$defaults$y_shift_step
    x_stretch <- if (!is.null(layer_def$x_stretch)) layer_def$x_stretch else stack$defaults$x_stretch
    y_stretch <- if (!is.null(layer_def$y_stretch)) layer_def$y_stretch else stack$defaults$y_stretch
    x_tilt <- if (!is.null(layer_def$x_tilt)) layer_def$x_tilt else stack$defaults$x_tilt
    y_tilt <- if (!is.null(layer_def$y_tilt)) layer_def$y_tilt else stack$defaults$y_tilt
    angle_rotate <- if (!is.null(layer_def$angle_rotate)) layer_def$angle_rotate else stack$defaults$angle_rotate

    # Update layer_def so later stages use the resolved values
    layer_def$x_shift <- x_shift
    layer_def$y_shift <- y_shift
    layer_def$x_stretch <- x_stretch
    layer_def$y_stretch <- y_stretch
    layer_def$x_tilt <- x_tilt
    layer_def$y_tilt <- y_tilt
    layer_def$angle_rotate <- angle_rotate

    stack$layers[[i]] <- layer_def

    # 1. Connectors
    if (i > 1 && length(stack$connectors) > 0) {
      prev_layer_def <- stack$layers[[i - 1]]
      for (conn in stack$connectors) {
        # Logic: Draw connector if it is global (pos 0 or N) 
        # or if it was placed specifically between the previous and current layer.
        is_global <- conn$pos == 0 || conn$pos == length(stack$layers)
        is_local_to_this_gap <- conn$pos == (i - 1)
        
        if (is_global || is_local_to_this_gap) {
          pts_prev <- tilt_map(conn$data, x_stretch = prev_layer_def$x_stretch, y_stretch = prev_layer_def$y_stretch, x_tilt = prev_layer_def$x_tilt, y_tilt = prev_layer_def$y_tilt, x_shift = prev_layer_def$x_shift, y_shift = prev_layer_def$y_shift, angle_rotate = prev_layer_def$angle_rotate)
          pts_curr <- tilt_map(conn$data, x_stretch = layer_def$x_stretch, y_stretch = layer_def$y_stretch, x_tilt = layer_def$x_tilt, y_tilt = layer_def$y_tilt, x_shift = layer_def$x_shift, y_shift = layer_def$y_shift, angle_rotate = layer_def$angle_rotate)

          lines_geom <- tilt_lines(pts_prev, pts_curr)
          p <- p + ggplot2::geom_sf(data = lines_geom, color = conn$color, alpha = conn$alpha, size = conn$size)
        }
      }
    }

    # 2. Map Layer
    tilted_data <- tilt_map(layer_def$data, x_stretch = layer_def$x_stretch, y_stretch = layer_def$y_stretch, x_tilt = layer_def$x_tilt, y_tilt = layer_def$y_tilt, x_shift = layer_def$x_shift, y_shift = layer_def$y_shift, angle_rotate = layer_def$angle_rotate)

    # Automatically fall back to NA if the requested fill column doesn't exist
    # or if it's not numeric (to avoid crashing continuous scales)
    if (!is.na(layer_def$fill)) {
      if (!(layer_def$fill %in% names(tilted_data)) || !is.numeric(tilted_data[[layer_def$fill]])) {
        layer_def$fill <- NA
      }
    }

    if (!is.na(layer_def$fill)) {
      if (has_mapped_fill) {
        p <- p + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
      }
      
      # We replicate plot_tiltedmaps logic for size if not provided
      layer_size <- if (!is.null(layer_def$size)) layer_def$size else (if (i == 1) 0.01 else 0.5)
      
      p <- p + ggplot2::geom_sf(data = tilted_data, ggplot2::aes(fill = .data[[layer_def$fill]], color = .data[[layer_def$fill]]), size = layer_size, alpha = layer_def$alpha)
      has_mapped_fill <- TRUE

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

    # 2.5 Annotations
    if (!is.na(layer_def$label)) {
      lbl_x <- if (!is.null(layer_def$label_x)) layer_def$label_x else if (!is.null(stack$defaults$label_x)) stack$defaults$label_x else global_xmin - 5
      lbl_y <- if (!is.null(layer_def$label_y)) layer_def$label_y else layer_def$y_shift
      lbl_col <- if (!is.null(layer_def$label_color)) layer_def$label_color else stack$defaults$label_color
      lbl_size <- if (!is.null(layer_def$label_size)) layer_def$label_size else stack$defaults$label_size
      
      # Use right justification (hjust = 1) if we are auto-calculating to avoid overlap
      lbl_hjust <- if (is.null(layer_def$label_x) && is.null(stack$defaults$label_x)) 1 else 0

      if (!is.null(lbl_x) && !is.null(lbl_y)) {
        p <- p + ggplot2::annotate("text", label = layer_def$label, x = lbl_x, y = lbl_y, color = lbl_col, size = lbl_size, hjust = lbl_hjust)
        has_labels <- TRUE
      }
    }
  }

  # 3. Top Points
  if (length(stack$connectors) > 0) {
    top_layer_def <- stack$layers[[length(stack$layers)]]
    for (conn in stack$connectors) {
      is_global <- conn$pos == 0 || conn$pos == length(stack$layers)
      is_top_layer_connector <- conn$pos == (length(stack$layers) - 1)
      
      if (conn$draw_points && (is_global || is_top_layer_connector)) {
        pts_top <- tilt_map(conn$data, x_stretch = top_layer_def$x_stretch, y_stretch = top_layer_def$y_stretch, x_tilt = top_layer_def$x_tilt, y_tilt = top_layer_def$y_tilt, x_shift = top_layer_def$x_shift, y_shift = top_layer_def$y_shift, angle_rotate = top_layer_def$angle_rotate)
        
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
