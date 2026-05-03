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
#'
#' @examples
#' \donttest{
#' # CRAN-compliant parallel example
#' if (requireNamespace("mirai", quietly = TRUE)) {
#'   # Try to set up a daemon, but handle cases where it might be blocked
#'   # (e.g., by some system security profiles)
#'   daemons_started <- tryCatch({
#'     mirai::daemons(1, dispatcher = FALSE)
#'     TRUE
#'   }, error = function(e) FALSE)
#'   
#'   if (daemons_started) {
#'     stack <- tilt_stack() |>
#'       tilt_layer(landscape_1) |>
#'       tilt_layer(landscape_2)
#'       
#'     anim <- animate_tilt_stack(stack, n_frames = 5)
#'     
#'     mirai::daemons(0)
#'     Sys.sleep(1)
#'   }
#' }
#' }
animate_tilt_stack <- function(stack, type = c("unfold", "reveal"), direction = c("up", "down"), n_frames = 50, ...) {
  rlang::check_installed("gganimate", reason = "to use `animate_tilt_stack()`.")
  
  type <- match.arg(type)
  direction <- match.arg(direction)
  
  if (length(stack$layers) == 0) return(ggplot2::ggplot() + ggplot2::theme_void())

  # Pre-convert all data to SF on the host for stability
  cli::cli_progress_step("Preparing spatial data")
  for (i in seq_along(stack$layers)) {
    d <- stack$layers[[i]]$data
    if (!inherits(d, "sf")) {
      stack$layers[[i]]$data <- sf::st_as_sf(stars::st_as_stars(d))
    }
  }

  status <- .get_mirai_status()
  
  if (status$active) {
    # Ensure workers have necessary packages loaded
    mirai::everywhere({
      loadNamespace("layer")
      loadNamespace("sf")
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
  layer_data_list <- lapply(stack$layers, `[[`, "data")

  if (status$active) {
    cli::cli_inform(c("i" = "Animation: Using Grouped Parallelism (Layer-bound)"))

    # Broadcast data once
    mirai::everywhere(
      {
        do.call("assign", list(".L_DATA", .d, envir = .GlobalEnv))
        do.call("assign", list(".L_PARAMS", .p, envir = .GlobalEnv))
        do.call("assign", list(".L_NFRAMES", .nf, envir = .GlobalEnv))
        do.call("assign", list(".L_FPL", .fpl, envir = .GlobalEnv))
      },
      .args = list(.d = layer_data_list, .p = params_final, .nf = n_frames, .fpl = frames_per_layer)
    )

    # Parallel path (one task per layer)
    results <- mirai::mirai_map(
      seq_along(stack$layers),
      function(i) {
        data <- .L_DATA[[i]]
        p <- .L_PARAMS[[i]]
        tilted_final <- layer::tilt_map(
          data,
          x_stretch = p$x_stretch, y_stretch = p$y_stretch,
          x_tilt = p$x_tilt, y_tilt = p$y_tilt,
          x_shift = p$x_shift, y_shift = p$y_shift,
          angle_rotate = p$angle_rotate
        )
        
        start_f <- (i - 1) * .L_FPL + 1
        layer_frames <- lapply(start_f:.L_NFRAMES, function(f) {
          tilted_f <- tilted_final
          tilted_f$.frame <- f
          tilted_f
        })
        do.call(rbind, layer_frames)
      }
    )
    
    tilted_finals <- results[mirai::.progress]
    mirai::everywhere({
      rm(list = c(".L_DATA", ".L_PARAMS", ".L_NFRAMES", ".L_FPL"), envir = .GlobalEnv)
    })
  } else {
    # Serial path (Existing logic)
    cli::cli_inform(c("i" = "Animation: Using Tier 3 (Serial Execution)"))
    pb <- cli::cli_progress_bar("Generating frames", total = n_layers)
    tilted_finals <- lapply(seq_along(stack$layers), function(i) {
      d <- layer_data_list[[i]]
      p <- params_final[[i]]
      tilted_final <- layer::tilt_map(
        d,
        x_stretch = p$x_stretch, y_stretch = p$y_stretch,
        x_tilt = p$x_tilt, y_tilt = p$y_tilt,
        x_shift = p$x_shift, y_shift = p$y_shift,
        angle_rotate = p$angle_rotate
      )
      
      start_f <- (i - 1) * frames_per_layer + 1
      layer_frames <- lapply(start_f:n_frames, function(f) {
        tilted_f <- tilted_final
        tilted_f$.frame <- f
        tilted_f
      })
      cli::cli_progress_update(id = pb)
      do.call(rbind, layer_frames)
    })
  }

  # Re-assemble
  p <- ggplot2::ggplot()
  has_mapped_fill <- FALSE
  for (i in seq_along(stack$layers)) {
    layer_def <- stack$layers[[i]]
    tilted_long <- tilted_finals[[i]]
    
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
  p + ggplot2::theme_void() + gganimate::transition_manual(.frame)
}

#' @noRd
.animate_unfold <- function(stack, direction, n_frames, status, ...) {
  params_final <- resolve_stack_params(stack)
  n_layers <- length(stack$layers)
  anchor_idx <- if (direction == "up") 1 else n_layers
  p_anchor <- params_final[[anchor_idx]]
  layer_data_list <- lapply(stack$layers, `[[`, "data")

  if (status$active) {
    cli::cli_inform(c("i" = "Animation: Using Grouped Parallelism (Layer-bound)"))

    # Broadcast data once
    mirai::everywhere(
      {
        do.call("assign", list(".L_DATA", .d, envir = .GlobalEnv))
        do.call("assign", list(".L_PARAMS", .p, envir = .GlobalEnv))
        do.call("assign", list(".L_ANCHOR", .pa, envir = .GlobalEnv))
        do.call("assign", list(".L_NFRAMES", .nf, envir = .GlobalEnv))
      },
      .args = list(.d = layer_data_list, .p = params_final, .pa = p_anchor, .nf = n_frames)
    )

    # Parallel path (one task per layer)
    results <- mirai::mirai_map(
      seq_along(stack$layers),
      function(i) {
        data <- .L_DATA[[i]]
        p_final <- .L_PARAMS[[i]]
        p_anchor <- .L_ANCHOR
        n_frames <- .L_NFRAMES
        
        layer_frames <- lapply(seq_len(n_frames), function(f) {
          weight <- (f - 1) / (n_frames - 1)
          curr_p <- list(
            x_shift = p_anchor$x_shift + (p_final$x_shift - p_anchor$x_shift) * weight,
            y_shift = p_anchor$y_shift + (p_final$y_shift - p_anchor$y_shift) * weight,
            x_stretch = 1 + (p_final$x_stretch - 1) * weight,
            y_stretch = 0 + (p_final$y_stretch - 0) * weight,
            x_tilt = 0 + (p_final$x_tilt - 0) * weight,
            y_tilt = 1 + (p_final$y_tilt - 1) * weight,
            angle_rotate = 0 + (p_final$angle_rotate - 0) * weight
          )
          tilted <- layer::tilt_map(
            data, 
            x_stretch = curr_p$x_stretch, y_stretch = curr_p$y_stretch, 
            x_tilt = curr_p$x_tilt, y_tilt = curr_p$y_tilt, 
            x_shift = curr_p$x_shift, y_shift = curr_p$y_shift, 
            angle_rotate = curr_p$angle_rotate
          )
          tilted$.frame <- f
          tilted$.layer <- i
          tilted
        })
        do.call(rbind, layer_frames)
      }
    )
    
    tilted_finals <- results[mirai::.progress]
    mirai::everywhere({
      rm(list = c(".L_DATA", ".L_PARAMS", ".L_ANCHOR", ".L_NFRAMES"), envir = .GlobalEnv)
    })

    if (any(sapply(tilted_finals, mirai::is_error_value))) {
      err_idx <- which(sapply(tilted_finals, mirai::is_error_value))[1]
      rlang::abort(c("x" = "Error in parallel animation frame generation.", 
                     "i" = sprintf("Worker message: %s", as.character(tilted_finals[[err_idx]]))))
    }
  } else {
    # Serial path
    cli::cli_inform(c("i" = "Animation: Using Tier 3 (Serial Execution)"))
    pb <- cli::cli_progress_bar("Generating frames", total = n_layers)
    tilted_finals <- lapply(seq_along(stack$layers), function(i) {
      data <- layer_data_list[[i]]
      p_final <- params_final[[i]]
      layer_frames <- lapply(seq_len(n_frames), function(f) {
        weight <- (f - 1) / (n_frames - 1)
        curr_p <- list(
          x_shift = p_anchor$x_shift + (p_final$x_shift - p_anchor$x_shift) * weight,
          y_shift = p_anchor$y_shift + (p_final$y_shift - p_anchor$y_shift) * weight,
          x_stretch = 1 + (p_final$x_stretch - 1) * weight,
          y_stretch = 0 + (p_final$y_stretch - 0) * weight,
          x_tilt = 0 + (p_final$x_tilt - 0) * weight,
          y_tilt = 1 + (p_final$y_tilt - 1) * weight,
          angle_rotate = 0 + (p_final$angle_rotate - 0) * weight
        )
        tilted <- layer::tilt_map(data, x_stretch = curr_p$x_stretch, y_stretch = curr_p$y_stretch, x_tilt = curr_p$x_tilt, y_tilt = curr_p$y_tilt, x_shift = curr_p$x_shift, y_shift = curr_p$y_shift, angle_rotate = curr_p$angle_rotate)
        tilted$.frame <- f
        tilted$.layer <- i
        tilted
      })
      cli::cli_progress_update(id = pb)
      do.call(rbind, layer_frames)
    })
  }

  # Re-assemble
  p <- ggplot2::ggplot()
  has_mapped_fill <- FALSE
  for (i in seq_along(stack$layers)) {
    layer_def <- stack$layers[[i]]
    tilted_long <- tilted_finals[[i]]
    
    # Rendering logic (Same as reveal)
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
  p + ggplot2::theme_void() + gganimate::transition_manual(.frame)
}
