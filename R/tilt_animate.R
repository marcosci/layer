#' Match data to a layer in a tilt stack
#'
#' This utility takes an `sf` or raster object and transforms it to perfectly align
#' with the spatial perspective of a specific layer in an existing `tilt_stack`.
#'
#' @param data An `sf` or raster object to transform.
#' @param stack A `tilt_stack` object.
#' @param layer The index of the layer in the stack to match.
#' @return A transformed `sf` object.
#' @export
#'
#' @examples
#' \donttest{
#' stack <- tilt_stack() |> tilt_layer(landscape_1)
#' points_tilted <- tilt_match(landscape_points, stack, layer = 1)
#' }
tilt_match <- function(data, stack, layer = 1) {
  if (layer < 1 || layer > length(stack$layers)) {
    rlang::abort(sprintf("Layer index %d is out of bounds (stack has %d layers).", layer, length(stack$layers)))
  }
  
  params <- resolve_stack_params(stack)
  p <- params[[layer]]
  
  tilt_map(
    data, 
    x_stretch = p$x_stretch, y_stretch = p$y_stretch, 
    x_tilt = p$x_tilt, y_tilt = p$y_tilt, 
    x_shift = p$x_shift, y_shift = p$y_shift, 
    angle_rotate = p$angle_rotate
  )
}

#' @noRd
.get_mirai_status <- function() {
  if (!requireNamespace("mirai", quietly = TRUE)) return(list(active = FALSE))
  
  # Check for active daemons using the official developer interface
  active <- mirai::daemons_set()
  
  if (!active) return(list(active = FALSE))
  
  # Check for mori
  has_mori <- requireNamespace("mori", quietly = TRUE)
  if (!has_mori) {
    cli::cli_inform(c(
      "!" = "Using {.pkg mirai} without {.pkg mori}.",
      "i" = "For significantly better performance and lower memory usage with large maps, install the {.pkg mori} package."
    ))
  }
  
  list(active = TRUE, has_mori = has_mori)
}

#' Animate a tilt stack
#'
#' Creates a `gganimate` object that visualizes the "infolding" or "revealing" of a map stack.
#'
#' @param stack A `tilt_stack` object.
#' @param type The type of animation: `"unfold"` (all layers expand from flat to 3D simultaneously) 
#'   or `"reveal"` (layers appear one by one).
#' @param direction The direction of the animation: `"up"` (bottom layer is fixed) or `"down"` (top layer is fixed).
#'   Currently only affects `"unfold"`.
#' @param n_frames Number of frames for the animation.
#' @param ... Additional arguments passed to `gganimate` functions.
#' @return A `gganim` object.
#' @export
animate_tilt_stack <- function(stack, type = c("unfold", "reveal"), direction = c("up", "down"), n_frames = 50, ...) {
  rlang::check_installed("gganimate", reason = "to use `animate_tilt_stack()`.")
  
  type <- match.arg(type)
  direction <- match.arg(direction)
  
  if (length(stack$layers) == 0) return(ggplot2::ggplot() + ggplot2::theme_void())

  status <- .get_mirai_status()
  
  if (status$active) {
    # Ensure workers have necessary packages
    mirai::everywhere({
      library(layer)
      library(sf)
      library(stars)
      library(raster)
    })
  }

  if (type == "reveal") {
    return(.animate_reveal(stack, direction, n_frames, status, ...))
  } else {
    return(.animate_unfold(stack, direction, n_frames, status, ...))
  }
}

#' @noRd
.animate_reveal <- function(stack, direction, n_frames, status, ...) {
  params_final <- resolve_stack_params(stack)
  n_layers <- length(stack$layers)
  frames_per_layer <- max(1, floor(n_frames / n_layers))

  # 1. Pre-convert all data to SF (Parallel-aware)
  cli::cli_progress_step("Preparing spatial data for workers")
  
  if (status$active) {
    # Parallel preparation of base SF objects
    layer_data_list <- mirai::mirai_map(
      stack$layers,
      function(l) {
        d <- l$data
        if (!inherits(d, "sf")) {
          d <- stars::st_as_stars(d)
          d <- sf::st_as_sf(d)
        }
        d
      }
    )[] 
  } else {
    # Serial preparation
    layer_data_list <- lapply(stack$layers, function(l) {
      d <- l$data
      if (!inherits(d, "sf")) {
        d <- stars::st_as_stars(d)
        d <- sf::st_as_sf(d)
      }
      d
    })
  }

  if (status$active) {
    # GLOBAL EXPORT STRATEGY
    if (status$has_mori) {
      cli::cli_inform(c("i" = "Animation: Using Tier 1 Parallelism (Zero-copy via {.pkg mori})"))
      layer_data_list <- lapply(layer_data_list, mori::share)
    } else {
      cli::cli_inform(c("i" = "Animation: Using Tier 2 Parallelism (Export-once via {.pkg mirai})"))
    }

    # Export to worker globals to avoid per-task serialization overhead
    mirai::everywhere(
      {
        .L_DATA <<- .d
        .L_PARAMS <<- .p
      },
      .args = list(.d = layer_data_list, .p = params_final)
    )

    # Parallel path (one task per layer)
    results <- mirai::mirai_map(
      seq_along(stack$layers),
      function(i) {
        data <- .L_DATA[[i]]
        p <- .L_PARAMS[[i]]
        layer::tilt_map(
          data,
          x_stretch = p$x_stretch, y_stretch = p$y_stretch,
          x_tilt = p$x_tilt, y_tilt = p$y_tilt,
          x_shift = p$x_shift, y_shift = p$y_shift,
          angle_rotate = p$angle_rotate
        )
      }
    )
    
    tilted_finals <- results[.progress]
    mirai::everywhere(rm(.L_DATA, .L_PARAMS))

    if (any(sapply(tilted_finals, mirai::is_error_value))) {
      err_idx <- which(sapply(tilted_finals, mirai::is_error_value))[1]
      rlang::abort(c("x" = "Error in parallel reveal animation.", 
                     "i" = sprintf("Worker message: %s", as.character(tilted_finals[[err_idx]]))))
    }
    
    # Re-assemble
    p <- ggplot2::ggplot()
    has_mapped_fill <- FALSE
    for (i in seq_along(stack$layers)) {
      layer_def <- stack$layers[[i]]
      tilted_final <- tilted_finals[[i]]
      start_f <- (i - 1) * frames_per_layer + 1
      layer_frames <- lapply(start_f:n_frames, function(f) {
        tilted_f <- tilted_final
        tilted_f$.frame <- f
        tilted_f
      })
      tilted_long <- do.call(rbind, layer_frames)
      
      # Rendering logic
      fill_col <- layer_def$fill
      if (!is.na(fill_col)) {
        if (!(fill_col %in% names(tilted_long)) || !is.numeric(tilted_long[[fill_col]])) fill_col <- NA
      }
      geom_type <- as.character(sf::st_geometry_type(tilted_long))[1]
      layer_size <- if (!is.null(layer_def$size)) layer_def$size else (if (i == 1) 0.01 else 0.5)

      if (!is.na(fill_col)) {
        if (has_mapped_fill) { p <- p + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color() }
        if (geom_type == "POINT") {
          p <- p + ggplot2::geom_sf(data = tilted_long, ggplot2::aes(fill = .data[[fill_col]], color = .data[[fill_col]], group = 1), size = layer_size, alpha = layer_def$alpha)
        } else {
          p <- p + ggplot2::geom_sf(data = tilted_long, ggplot2::aes(fill = .data[[fill_col]], color = .data[[fill_col]], group = 1), linewidth = layer_size, alpha = layer_def$alpha)
        }
        has_mapped_fill <- TRUE
        if (layer_def$palette %in% c("viridis", "inferno", "magma", "plasma", "cividis", "mako", "rocket", "turbo", letters[1:9])) {
          p <- p + ggplot2::scale_fill_viridis_c(option = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none") +
                   ggplot2::scale_color_viridis_c(option = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none")
        } else if (layer_def$palette %in% scico::scico_palette_names()) {
          p <- p + scico::scale_fill_scico(palette = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none") +
                   scico::scale_color_scico(palette = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none")
        }
      } else {
        if (geom_type == "POINT") {
          p <- p + ggplot2::geom_sf(data = tilted_long, color = layer_def$color, alpha = layer_def$alpha, size = layer_size, group = 1)
        } else {
          p <- p + ggplot2::geom_sf(data = tilted_long, color = layer_def$color, alpha = layer_def$alpha, linewidth = layer_size, group = 1)
        }
      }
    }
  } else {
    # Serial path
    cli::cli_inform(c("i" = "Animation: Using Tier 3 (Serial Execution)"))
    p <- ggplot2::ggplot()
    has_mapped_fill <- FALSE
    pb <- cli::cli_progress_bar("Generating frames", total = n_layers)
    for (i in seq_along(stack$layers)) {
      layer_def <- stack$layers[[i]]
      p_final <- params_final[[i]]
      tilted_final <- tilt_map(layer_data_list[[i]], x_stretch = p_final$x_stretch, y_stretch = p_final$y_stretch, x_tilt = p_final$x_tilt, y_tilt = p_final$y_tilt, x_shift = p_final$x_shift, y_shift = p_final$y_shift, angle_rotate = p_final$angle_rotate)
      start_f <- (i - 1) * frames_per_layer + 1
      layer_frames <- lapply(start_f:n_frames, function(f) {
        tilted_f <- tilted_final
        tilted_f$.frame <- f
        tilted_f
      })
      tilted_long <- do.call(rbind, layer_frames)
      # (Same rendering logic)
      fill_col <- layer_def$fill
      if (!is.na(fill_col)) {
        if (!(fill_col %in% names(tilted_long)) || !is.numeric(tilted_long[[fill_col]])) fill_col <- NA
      }
      geom_type <- as.character(sf::st_geometry_type(tilted_long))[1]
      layer_size <- if (!is.null(layer_def$size)) layer_def$size else (if (i == 1) 0.01 else 0.5)
      if (!is.na(fill_col)) {
        if (has_mapped_fill) { p <- p + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color() }
        if (geom_type == "POINT") {
          p <- p + ggplot2::geom_sf(data = tilted_long, ggplot2::aes(fill = .data[[fill_col]], color = .data[[fill_col]], group = 1), size = layer_size, alpha = layer_def$alpha)
        } else {
          p <- p + ggplot2::geom_sf(data = tilted_long, ggplot2::aes(fill = .data[[fill_col]], color = .data[[fill_col]], group = 1), linewidth = layer_size, alpha = layer_def$alpha)
        }
        has_mapped_fill <- TRUE
        if (layer_def$palette %in% c("viridis", "inferno", "magma", "plasma", "cividis", "mako", "rocket", "turbo", letters[1:9])) {
          p <- p + ggplot2::scale_fill_viridis_c(option = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none") +
                   ggplot2::scale_color_viridis_c(option = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none")
        } else if (layer_def$palette %in% scico::scico_palette_names()) {
          p <- p + scico::scale_fill_scico(palette = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none") +
                   scico::scale_color_scico(palette = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none")
        }
      } else {
        if (geom_type == "POINT") {
          p <- p + ggplot2::geom_sf(data = tilted_long, color = layer_def$color, alpha = layer_def$alpha, size = layer_size, group = 1)
        } else {
          p <- p + ggplot2::geom_sf(data = tilted_long, color = layer_def$color, alpha = layer_def$alpha, linewidth = layer_size, group = 1)
        }
      }
      cli::cli_progress_update(id = pb)
    }
  }
  p + ggplot2::theme_void() + gganimate::transition_manual(.frame)
}

#' @noRd
.animate_unfold <- function(stack, direction, n_frames, status, ...) {
  params_final <- resolve_stack_params(stack)
  n_layers <- length(stack$layers)
  anchor_idx <- if (direction == "up") 1 else n_layers
  p_anchor <- params_final[[anchor_idx]]

  # 1. Pre-convert all data to SF (Parallel-aware)
  cli::cli_progress_step("Preparing spatial data for workers")
  
  if (status$active) {
    # Parallel preparation
    layer_data_list <- mirai::mirai_map(
      stack$layers,
      function(l) {
        d <- l$data
        if (!inherits(d, "sf")) {
          d <- stars::st_as_stars(d)
          d <- sf::st_as_sf(d)
        }
        d
      }
    )[] 
  } else {
    # Serial preparation
    layer_data_list <- lapply(stack$layers, function(l) {
      d <- l$data
      if (!inherits(d, "sf")) {
        d <- stars::st_as_stars(d)
        d <- sf::st_as_sf(d)
      }
      d
    })
  }

  if (status$active) {
    # GLOBAL EXPORT STRATEGY
    tasks <- expand.grid(frame = seq_len(n_frames), layer = seq_len(n_layers))
    tasks_list <- split(tasks, seq_len(nrow(tasks)))
    
    if (status$has_mori) {
      cli::cli_inform(c("i" = "Animation: Using Tier 1 Parallelism (Zero-copy via {.pkg mori})"))
      layer_data_list <- lapply(layer_data_list, mori::share)
    } else {
      cli::cli_inform(c("i" = "Animation: Using Tier 2 Parallelism (Export-once via {.pkg mirai})"))
    }

    # Export to worker globals
    mirai::everywhere(
      {
        .L_DATA <<- .d
        .L_PARAMS <<- .p
        .L_ANCHOR <<- .a
      },
      .args = list(.d = layer_data_list, .p = params_final, .a = p_anchor)
    )

    # Parallel path
    results <- mirai::mirai_map(
      tasks_list,
      function(row) {
        f <- row$frame
        i <- row$layer
        data <- .L_DATA[[i]]
        p_final <- .L_PARAMS[[i]]
        p_anchor <- .L_ANCHOR
        
        weight <- (f - 1) / (n_frames - 1)
        curr_p <- list(
          x_shift = p_anchor$x_shift + (p_final$x_shift - p_anchor$x_shift) * weight,
          y_shift = p_anchor$y_shift + (p_final$y_shift - p_anchor$y_shift) * weight,
          x_stretch = 1 + (p_final$x_stretch - 1) * weight,
          y_stretch = 1 + (p_final$y_stretch - 1) * weight,
          x_tilt = 0 + (p_final$x_tilt - 0) * weight,
          y_tilt = 0 + (p_final$y_tilt - 0) * weight,
          angle_rotate = 0 + (p_final$angle_rotate - 0) * weight
        )
        tilted <- layer::tilt_map(data, x_stretch = curr_p$x_stretch, y_stretch = curr_p$y_stretch, x_tilt = curr_p$x_tilt, y_tilt = curr_p$y_tilt, x_shift = curr_p$x_shift, y_shift = curr_p$y_shift, angle_rotate = curr_p$angle_rotate)
        tilted$.frame <- f
        tilted$.layer <- i
        tilted
      },
      n_frames = n_frames
    )
    
    all_tilted <- results[.progress]
    mirai::everywhere(rm(.L_DATA, .L_PARAMS, .L_ANCHOR))

    if (any(sapply(all_tilted, mirai::is_error_value))) {
      err_idx <- which(sapply(all_tilted, mirai::is_error_value))[1]
      rlang::abort(c("x" = "Error in parallel animation frame generation.", 
                     "i" = sprintf("Worker message: %s", as.character(all_tilted[[err_idx]]))))
    }

    # Re-assemble
    p <- ggplot2::ggplot()
    has_mapped_fill <- FALSE
    for (i in seq_len(n_layers)) {
      layer_def <- stack$layers[[i]]
      layer_frames <- all_tilted[tasks$layer == i]
      tilted_long <- do.call(rbind, layer_frames)
      
      # Rendering logic
      fill_col <- layer_def$fill
      if (!is.na(fill_col)) {
        if (!(fill_col %in% names(tilted_long)) || !is.numeric(tilted_long[[fill_col]])) fill_col <- NA
      }
      geom_type <- as.character(sf::st_geometry_type(tilted_long))[1]
      layer_size <- if (!is.null(layer_def$size)) layer_def$size else (if (i == 1) 0.01 else 0.5)

      if (!is.na(fill_col)) {
        if (has_mapped_fill) { p <- p + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color() }
        if (geom_type == "POINT") {
          p <- p + ggplot2::geom_sf(data = tilted_long, ggplot2::aes(fill = .data[[fill_col]], color = .data[[fill_col]], group = 1), size = layer_size, alpha = layer_def$alpha)
        } else {
          p <- p + ggplot2::geom_sf(data = tilted_long, ggplot2::aes(fill = .data[[fill_col]], color = .data[[fill_col]], group = 1), linewidth = layer_size, alpha = layer_def$alpha)
        }
        has_mapped_fill <- TRUE
        if (layer_def$palette %in% c("viridis", "inferno", "magma", "plasma", "cividis", "mako", "rocket", "turbo", letters[1:9])) {
          p <- p + ggplot2::scale_fill_viridis_c(option = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none") +
                   ggplot2::scale_color_viridis_c(option = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none")
        } else if (layer_def$palette %in% scico::scico_palette_names()) {
          p <- p + scico::scale_fill_scico(palette = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none") +
                   scico::scale_color_scico(palette = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none")
        }
      } else {
        if (geom_type == "POINT") {
          p <- p + ggplot2::geom_sf(data = tilted_long, color = layer_def$color, alpha = layer_def$alpha, size = layer_size, group = 1)
        } else {
          p <- p + ggplot2::geom_sf(data = tilted_long, color = layer_def$color, alpha = layer_def$alpha, linewidth = layer_size, group = 1)
        }
      }
    }
  } else {
    # Serial path
    cli::cli_inform(c("i" = "Animation: Using Tier 3 (Serial Execution)"))
    p <- ggplot2::ggplot()
    has_mapped_fill <- FALSE
    pb <- cli::cli_progress_bar("Generating frames", total = n_layers * n_frames)
    for (i in seq_along(stack$layers)) {
      layer_def <- stack$layers[[i]]
      p_final <- params_final[[i]]
      layer_frames <- lapply(seq_len(n_frames), function(f) {
        weight <- (f - 1) / (n_frames - 1)
        curr_p <- list(
          x_shift = p_anchor$x_shift + (p_final$x_shift - p_anchor$x_shift) * weight,
          y_shift = p_anchor$y_shift + (p_final$y_shift - p_anchor$y_shift) * weight,
          x_stretch = 1 + (p_final$x_stretch - 1) * weight,
          y_stretch = 1 + (p_final$y_stretch - 1) * weight,
          x_tilt = 0 + (p_final$x_tilt - 0) * weight,
          y_tilt = 0 + (p_final$y_tilt - 0) * weight,
          angle_rotate = 0 + (p_final$angle_rotate - 0) * weight
        )
        tilted <- tilt_map(layer_data_list[[i]], x_stretch = curr_p$x_stretch, y_stretch = curr_p$y_stretch, x_tilt = curr_p$x_tilt, y_tilt = curr_p$y_tilt, x_shift = curr_p$x_shift, y_shift = curr_p$y_shift, angle_rotate = curr_p$angle_rotate)
        tilted$.frame <- f
        cli::cli_progress_update(id = pb)
        tilted
      })
      tilted_long <- do.call(rbind, layer_frames)
      # Rendering logic (Same as above)
      fill_col <- layer_def$fill
      if (!is.na(fill_col)) {
        if (!(fill_col %in% names(tilted_long)) || !is.numeric(tilted_long[[fill_col]])) fill_col <- NA
      }
      geom_type <- as.character(sf::st_geometry_type(tilted_long))[1]
      layer_size <- if (!is.null(layer_def$size)) layer_def$size else (if (i == 1) 0.01 else 0.5)
      if (!is.na(fill_col)) {
        if (has_mapped_fill) { p <- p + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color() }
        if (geom_type == "POINT") {
          p <- p + ggplot2::geom_sf(data = tilted_long, ggplot2::aes(fill = .data[[fill_col]], color = .data[[fill_col]], group = 1), size = layer_size, alpha = layer_def$alpha)
        } else {
          p <- p + ggplot2::geom_sf(data = tilted_long, ggplot2::aes(fill = .data[[fill_col]], color = .data[[fill_col]], group = 1), linewidth = layer_size, alpha = layer_def$alpha)
        }
        has_mapped_fill <- TRUE
        if (layer_def$palette %in% c("viridis", "inferno", "magma", "plasma", "cividis", "mako", "rocket", "turbo", letters[1:9])) {
          p <- p + ggplot2::scale_fill_viridis_c(option = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none") +
                   ggplot2::scale_color_viridis_c(option = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none")
        } else if (layer_def$palette %in% scico::scico_palette_names()) {
          p <- p + scico::scale_fill_scico(palette = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none") +
                   scico::scale_color_scico(palette = layer_def$palette, direction = layer_def$direction, begin = layer_def$begin, end = layer_def$end, alpha = layer_def$alpha, guide = "none")
        }
      } else {
        if (geom_type == "POINT") {
          p <- p + ggplot2::geom_sf(data = tilted_long, color = layer_def$color, alpha = layer_def$alpha, size = layer_size, group = 1)
        } else {
          p <- p + ggplot2::geom_sf(data = tilted_long, color = layer_def$color, alpha = layer_def$alpha, linewidth = layer_size, group = 1)
        }
      }
    }
  }
  p + ggplot2::theme_void() + gganimate::transition_manual(.frame)
}
