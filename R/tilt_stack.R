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
#' @param label_align Default alignment for labels: `"stack"` (default) aligns all labels to the leftmost edge of the entire stack; `"layer"` aligns each label to the leftmost edge of its specific layer.
#' @param label_color Default color for layer labels.
#' @param label_size Default size for layer labels.
#' @param label_family Default font family for layer labels.
#' @param label_fontface Default font face for layer labels.
#' @param label_alpha Default alpha for layer labels.
#' @param label_hjust Default horizontal justification for layer labels.
#' @param label_vjust Default vertical justification for layer labels.
#' @param label_lineheight Default line height for layer labels.
#' @return An object of class `tilt_stack`.
#' @export
#'
#' @examples
#' tilt_stack()
tilt_stack <- function(
  x_shift_step = 0, y_shift_step = 0,
  x_stretch = 2, y_stretch = 1.2, x_tilt = 0, y_tilt = 1, angle_rotate = pi/20,
  label_x = NULL, label_y = NULL, label_align = "stack",
  label_color = "grey40", label_size = 4,
  label_family = "", label_fontface = "plain", label_alpha = 1,
  label_hjust = NULL, label_vjust = 0.5, label_lineheight = 1.2
) {
  structure(list(
    layers = list(), 
    connectors = list(),
    defaults = list(
      x_shift_step = x_shift_step, y_shift_step = y_shift_step,
      x_stretch = x_stretch, y_stretch = y_stretch, 
      x_tilt = x_tilt, y_tilt = y_tilt, angle_rotate = angle_rotate,
      label_x = label_x, label_y = label_y, label_align = label_align,
      label_color = label_color, label_size = label_size,
      label_family = label_family, label_fontface = label_fontface, label_alpha = label_alpha,
      label_hjust = label_hjust, label_vjust = label_vjust, label_lineheight = label_lineheight
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
#' @param size Size/linewidth of the points or lines in the layer. If `NULL`, defaults to `0.01` for the first layer and `0.5` for others.
#' @param label Optional text label to annotate the layer.
#' @param label_x X coordinate for the label. If `NULL`, inherits from stack defaults.
#' @param label_y Y coordinate for the label. If `NULL`, inherits from stack defaults or `y_shift`.
#' @param label_align Alignment for the label: `"stack"` or `"layer"`. If `NULL`, inherits from stack defaults.
#' @param label_color Color for the label. If `NULL`, inherits from stack defaults.
#' @param label_size Size for the label. If `NULL`, inherits from stack defaults.
#' @param label_family Font family for the label. If `NULL`, inherits from stack defaults.
#' @param label_fontface Font face for the label. If `NULL`, inherits from stack defaults.
#' @param label_alpha Alpha for the label. If `NULL`, inherits from stack defaults.
#' @param label_hjust Horizontal justification for the label. If `NULL`, inherits from stack defaults.
#' @param label_vjust Vertical justification for the label. If `NULL`, inherits from stack defaults.
#' @param label_lineheight Line height for the label. If `NULL`, inherits from stack defaults.
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
  label = NA, label_x = NULL, label_y = NULL, label_align = NULL,
  label_color = NULL, label_size = NULL,
  label_family = NULL, label_fontface = NULL, label_alpha = NULL,
  label_hjust = NULL, label_vjust = NULL, label_lineheight = NULL
) {
  layer_def <- list(
    data = data,
    x_shift = x_shift, y_shift = y_shift,
    x_shift_rel = x_shift_rel, y_shift_rel = y_shift_rel,
    x_stretch = x_stretch, y_stretch = y_stretch, x_tilt = x_tilt, y_tilt = y_tilt, angle_rotate = angle_rotate,
    fill = fill, color = color, palette = palette, direction = direction, begin = begin, end = end, alpha = alpha,
    size = size,
    label = label, label_x = label_x, label_y = label_y, label_align = label_align,
    label_color = label_color, label_size = label_size,
    label_family = label_family, label_fontface = label_fontface, label_alpha = label_alpha,
    label_hjust = label_hjust, label_vjust = label_vjust, label_lineheight = label_lineheight
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
#' @param linewidth Thickness of the connector lines. Default is `1.0`.
#' @param linetype Line type of the connector lines (e.g. "solid", "dashed").
#' @param draw_points Logical; if `TRUE`, draws the points on the top layer.
#' @param point_color Color of the points drawn on top.
#' @param point_size Size of the points drawn on top.
#' @param point_shape Shape of the points drawn on top.
#' @param point_fill Fill color of the points drawn on top (for shapes 21-25).
#' @param point_alpha Opacity of the points drawn on top.
#' @param point_stroke Border thickness of the points drawn on top (for shapes 21-25).
#' @param on_top Logical; if `TRUE`, connector lines are drawn on top of all layers. If `FALSE` (default), lines are interleaved between layers for perfect 3D occlusion.
#'
#' @return The updated `tilt_stack`.
#' @export
tilt_connector <- function(
  stack, data,
  color = "grey60", alpha = 1, linewidth = 1.0, linetype = "solid",
  draw_points = TRUE, point_color = "black", point_size = 1,
  point_shape = 19, point_fill = NULL, point_alpha = 1, point_stroke = 0.5,
  on_top = FALSE
) {
  # Pre-calculate centroids to speed up tilt_map operations later
  geom_types <- as.character(sf::st_geometry_type(data))
  if (any(geom_types != "POINT")) {
    data <- suppressWarnings(sf::st_centroid(data))
  }

  conn_def <- list(
    data = data,
    color = color, alpha = alpha, linewidth = linewidth, linetype = linetype,
    draw_points = draw_points, point_color = point_color, point_size = point_size,
    point_shape = point_shape, point_fill = point_fill, point_alpha = point_alpha,
    point_stroke = point_stroke,
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

  # 2. Pre-evaluation Phase: Tilt all layers and connectors ONCE
  tilted_layers <- lapply(seq_along(stack$layers), function(i) {
    l <- stack$layers[[i]]
    p <- params[[i]]
    tilt_map(l$data, x_stretch = p$x_stretch, y_stretch = p$y_stretch, x_tilt = p$x_tilt, y_tilt = p$y_tilt, x_shift = p$x_shift, y_shift = p$y_shift, angle_rotate = p$angle_rotate)
  })

  tilted_connectors <- lapply(seq_along(stack$connectors), function(j) {
    conn <- stack$connectors[[j]]
    lapply(seq_along(stack$layers), function(i) {
      p <- params[[i]]
      tilt_map(conn$data, x_stretch = p$x_stretch, y_stretch = p$y_stretch, x_tilt = p$x_tilt, y_tilt = p$y_tilt, x_shift = p$x_shift, y_shift = p$y_shift, angle_rotate = p$angle_rotate)
    })
  })

  # 3. Robust xmin for label positioning
  global_xmin <- if (is.null(stack$defaults$label_x)) {
    min(sapply(tilted_layers, function(x) sf::st_bbox(x)["xmin"]))
  } else {
    stack$defaults$label_x
  }

  p <- ggplot2::ggplot()
  has_mapped_fill <- FALSE
  has_labels <- FALSE
  connector_layers <- list()

  # 4. Main Rendering Loop
  for (i in seq_along(stack$layers)) {
    layer_def <- stack$layers[[i]]
    p_curr <- params[[i]]
    tilted_data <- tilted_layers[[i]]

    # Fallback if fill column is missing or non-numeric
    if (!is.na(layer_def$fill)) {
      if (!(layer_def$fill %in% names(tilted_data)) || !is.numeric(tilted_data[[layer_def$fill]])) {
        layer_def$fill <- NA
      }
    }

    # Draw Map Layer
    geom_type <- as.character(sf::st_geometry_type(tilted_data))[1]
    if (!is.na(layer_def$fill)) {
      if (has_mapped_fill) {
        p <- p + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
      }
      layer_size <- if (!is.null(layer_def$size)) layer_def$size else (if (i == 1) 0.01 else 0.5)
      
      if (geom_type == "POINT") {
        p <- p + ggplot2::geom_sf(data = tilted_data, ggplot2::aes(fill = .data[[layer_def$fill]], color = .data[[layer_def$fill]]), size = layer_size, alpha = layer_def$alpha)
      } else {
        p <- p + ggplot2::geom_sf(data = tilted_data, ggplot2::aes(fill = .data[[layer_def$fill]], color = .data[[layer_def$fill]]), linewidth = layer_size, alpha = layer_def$alpha)
      }
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
      if (geom_type == "POINT") {
        p <- p + ggplot2::geom_sf(data = tilted_data, color = layer_def$color, alpha = layer_def$alpha, size = layer_size)
      } else {
        p <- p + ggplot2::geom_sf(data = tilted_data, color = layer_def$color, alpha = layer_def$alpha, linewidth = layer_size)
      }
    }

    # 5. Collect Connectors
    if (i < length(stack$layers) && length(stack$connectors) > 0) {
      for (j in seq_along(stack$connectors)) {
        conn <- stack$connectors[[j]]
        is_global <- conn$pos == 0 || conn$pos == length(stack$layers)
        is_local_to_this_gap <- conn$pos == i
        
        if (is_global || is_local_to_this_gap) {
          pts_curr <- tilted_connectors[[j]][[i]]
          pts_next <- tilted_connectors[[j]][[i + 1]]
          
          lines_geom <- tilt_lines(pts_curr, pts_next)
          
          if (conn$on_top) {
            # Defer to end
            connector_layers[[length(connector_layers) + 1]] <- ggplot2::geom_sf(
              data = lines_geom, 
              color = conn$color, 
              alpha = conn$alpha, 
              linewidth = conn$linewidth,
              linetype = conn$linetype
            )
          } else {
            # Draw now (so next map layer can occlude it)
            p <- p + ggplot2::geom_sf(
              data = lines_geom, 
              color = conn$color, 
              alpha = conn$alpha, 
              linewidth = conn$linewidth,
              linetype = conn$linetype
            )
          }
        }
      }
    }

    # 6. Annotations
    if (!is.na(layer_def$label)) {
      align_mode <- if (!is.null(layer_def$label_align)) layer_def$label_align else stack$defaults$label_align
      
      lbl_x <- if (!is.null(layer_def$label_x)) {
        layer_def$label_x 
      } else if (align_mode == "layer") {
        sf::st_bbox(tilted_data)["xmin"] - 5
      } else { # "stack"
        global_xmin - 5
      }
      
      lbl_y <- if (!is.null(layer_def$label_y)) layer_def$label_y else p_curr$y_shift
      lbl_col <- if (!is.null(layer_def$label_color)) layer_def$label_color else stack$defaults$label_color
      lbl_size <- if (!is.null(layer_def$label_size)) layer_def$label_size else stack$defaults$label_size
      
      # Typography overrides
      lbl_fam <- if (!is.null(layer_def$label_family)) layer_def$label_family else stack$defaults$label_family
      lbl_face <- if (!is.null(layer_def$label_fontface)) layer_def$label_fontface else stack$defaults$label_fontface
      lbl_alpha <- if (!is.null(layer_def$label_alpha)) layer_def$label_alpha else stack$defaults$label_alpha
      lbl_vjust <- if (!is.null(layer_def$label_vjust)) layer_def$label_vjust else stack$defaults$label_vjust
      lbl_lh <- if (!is.null(layer_def$label_lineheight)) layer_def$label_lineheight else stack$defaults$label_lineheight
      
      # Smart hjust
      lbl_hjust <- if (!is.null(layer_def$label_hjust)) {
        layer_def$label_hjust 
      } else if (!is.null(stack$defaults$label_hjust)) {
        stack$defaults$label_hjust
      } else if (is.null(layer_def$label_x) && is.null(stack$defaults$label_x)) {
        1 # Auto-align to left of stack
      } else {
        0 # Default left-aligned
      }

      if (!is.null(lbl_x) && !is.null(lbl_y)) {
        p <- p + ggplot2::annotate(
          "text", 
          label = layer_def$label, 
          x = lbl_x, y = lbl_y, 
          color = lbl_col, 
          size = lbl_size, 
          hjust = lbl_hjust,
          vjust = lbl_vjust,
          family = lbl_fam,
          fontface = lbl_face,
          alpha = lbl_alpha,
          lineheight = lbl_lh
        )
        has_labels <- TRUE
      }
    }
  }

  # 7. Draw all connector lines on top
  for (cl in connector_layers) {
    p <- p + cl
  }

  # 8. Draw Top Points
  if (length(stack$connectors) > 0) {
    p_top <- params[[length(params)]]
    for (j in seq_along(stack$connectors)) {
      conn <- stack$connectors[[j]]
      is_global <- conn$pos == 0 || conn$pos == length(stack$layers)
      is_top_layer_connector <- conn$pos == (length(stack$layers) - 1)
      
      if (conn$draw_points && (is_global || is_top_layer_connector)) {
        pts_top <- tilted_connectors[[j]][[length(tilted_connectors[[j]])]]
        
        # Build geom_sf call dynamically to avoid "Ignoring empty aesthetic: fill" warnings
        point_args <- list(
          data = pts_top, 
          color = conn$point_color, 
          size = conn$point_size,
          shape = conn$point_shape,
          alpha = conn$point_alpha,
          stroke = conn$point_stroke
        )
        if (!is.null(conn$point_fill)) point_args$fill <- conn$point_fill
        
        p <- p + do.call(ggplot2::geom_sf, point_args)
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
