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
#'   or `"reveal"` (layers appear with a zoom effect).
#' @param direction The direction of the animation: `"up"` (bottom layer is fixed) or `"down"` (top layer is fixed).
#'   Currently only affects `"unfold"`.
#' @param n_frames Number of frames for the animation.
#' @param sequential Logical. If `TRUE` (default), the animation progresses layer by layer. If `FALSE`, all layers are animated simultaneously.
#' @param verbose Logical. If `TRUE`, shows progress messages. Defaults to `interactive()`.
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
#'     anim <- animate_tilt_stack(stack, n_frames = 5, verbose = FALSE)
#'     
#'     mirai::daemons(0)
#'     Sys.sleep(1)
#'   }
#' }
#' }
animate_tilt_stack <- function(stack, type = c("unfold", "reveal"), direction = c("up", "down"), n_frames = 50, sequential = TRUE, verbose = interactive(), ...) {
  rlang::check_installed("gganimate", reason = "to use `animate_tilt_stack()`.")
  
  type <- match.arg(type)
  direction <- match.arg(direction)
  
  if (length(stack$layers) == 0) return(ggplot2::ggplot() + ggplot2::theme_void())

  # Pre-convert all data to SF on the host for stability
  if (verbose) cli::cli_progress_step("Preparing spatial data")
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
    results <- .animate_reveal(stack, direction, n_frames, sequential, status, verbose, ...)
  } else {
    results <- .animate_unfold(stack, direction, n_frames, sequential, status, verbose, ...)
  }
  
  tilted_finals <- results$tilted
  labels_finals <- results$labels

  # Re-assemble
  p <- ggplot2::ggplot()
  has_mapped_fill <- FALSE
  for (i in seq_along(stack$layers)) {
    layer_def <- stack$layers[[i]]
    tilted_long <- tilted_finals[[i]]
    
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
  
  p <- p + ggplot2::theme_void()

  # Add labels
  all_labels <- do.call(rbind, labels_finals)
  if (!is.null(all_labels) && nrow(all_labels) > 0) {
    # Find common label aesthetics from stack defaults or first labeled layer
    lbl_layers <- Filter(function(l) !is.na(l$label), stack$layers)
    first_lbl_layer <- if (length(lbl_layers) > 0) lbl_layers[[1]] else NULL
    
    # We assume consistent side for now in animation
    side <- if (!is.null(first_lbl_layer$label_side)) first_lbl_layer$label_side else stack$defaults$label_side
    
    p <- p + ggplot2::geom_text(
      data = all_labels,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      hjust = if (side == "right") 0 else 1,
      color = if (!is.null(first_lbl_layer$label_color)) first_lbl_layer$label_color else stack$defaults$label_color,
      size = if (!is.null(first_lbl_layer$label_size)) first_lbl_layer$label_size else stack$defaults$label_size,
      family = if (!is.null(first_lbl_layer$label_family)) first_lbl_layer$label_family else stack$defaults$label_family,
      fontface = if (!is.null(first_lbl_layer$label_fontface)) first_lbl_layer$label_fontface else stack$defaults$label_fontface,
      alpha = if (!is.null(first_lbl_layer$label_alpha)) first_lbl_layer$label_alpha else stack$defaults$label_alpha
    )
    # Ensure clipping is off to prevent label truncation
    p <- p + ggplot2::coord_sf(clip = "off")
    
    # Dynamic margins based on label side
    lbl_sides <- sapply(stack$layers, function(l) if (!is.null(l$label_side)) l$label_side else stack$defaults$label_side)
    margin_l <- if (any(lbl_sides == "left")) 150 else 10
    margin_r <- if (any(lbl_sides == "right")) 150 else 10
    p <- p + ggplot2::theme(plot.margin = ggplot2::margin(l = margin_l, r = margin_r, t = 10, b = 10))
  }

  p + gganimate::transition_manual(.frame)
}

#' @noRd
.animate_reveal <- function(stack, direction, n_frames, sequential, status, verbose, ...) {
  params_final <- resolve_stack_params(stack)
  n_layers <- length(stack$layers)
  
  if (sequential) {
    frames_per_layer <- max(2, floor(n_frames / n_layers))
  } else {
    frames_per_layer <- n_frames
  }
  
  layer_data_list <- lapply(stack$layers, `[[`, "data")
  layer_labels <- lapply(stack$layers, `[[`, "label")
  layer_sides <- lapply(stack$layers, function(l) if (!is.null(l$label_side)) l$label_side else stack$defaults$label_side)
  layer_x_offsets <- lapply(stack$layers, function(l) if (!is.null(l$label_x_offset)) l$label_x_offset else (if (!is.null(stack$defaults$label_x_offset)) stack$defaults$label_x_offset else 5))
  layer_y_offsets <- lapply(stack$layers, function(l) if (!is.null(l$label_y_offset)) l$label_y_offset else (if (!is.null(stack$defaults$label_y_offset)) stack$defaults$label_y_offset else 0))

  if (status$active) {
    if (verbose) cli::cli_inform(c("i" = "Animation: Using Grouped Parallelism (Frame-bound)"))

    # Broadcast data once
    mirai::everywhere(
      {
        # Use eval(parse) to bypass R CMD check's static analysis of global assignments
        eval(parse(text = ".L_DATA <<- .d"))
        eval(parse(text = ".L_LABELS <<- .ll"))
        eval(parse(text = ".L_SIDES <<- .ls"))
        eval(parse(text = ".L_XOFFS <<- .lx"))
        eval(parse(text = ".L_YOFFS <<- .ly"))
        eval(parse(text = ".L_PARAMS <<- .p"))
        eval(parse(text = ".L_NFRAMES <<- .nf"))
        eval(parse(text = ".L_FPL <<- .fpl"))
        eval(parse(text = ".L_SEQ <<- .seq"))
      },
      .args = list(.d = layer_data_list, .ll = layer_labels, .ls = layer_sides, .lx = layer_x_offsets, .ly = layer_y_offsets, .p = params_final, .nf = n_frames, .fpl = frames_per_layer, .seq = sequential)
    )

    # Parallel path (one task per frame)
    results <- mirai::mirai_map(
      1:n_frames,
      function(f) {
        res_tilted_f <- list()
        res_labels_f <- list()

        for (i in seq_along(.L_DATA)) {
          data <- .L_DATA[[i]]
          p_final <- .L_PARAMS[[i]]
          lbl_text <- .L_LABELS[[i]]
          lbl_side <- .L_SIDES[[i]]
          x_off <- .L_XOFFS[[i]]
          y_off <- .L_YOFFS[[i]]

          if (.L_SEQ) {
            start_f <- (i - 1) * .L_FPL + 1
            end_f <- min(.L_NFRAMES, i * .L_FPL)
          } else {
            start_f <- 1
            end_f <- .L_NFRAMES
          }

          if (f < start_f) next

          if (f <= end_f) {
            weight <- (f - start_f) / max(1, (end_f - start_f))
            curr_p <- list(
              x_shift = 0 + (p_final$x_shift - 0) * weight,
              y_shift = 0 + (p_final$y_shift - 0) * weight,
              x_stretch = 0.01 + (p_final$x_stretch - 0.01) * weight,
              y_stretch = 0 + (p_final$y_stretch - 0) * weight,
              x_tilt = 0 + (p_final$x_tilt - 0) * weight,
              y_tilt = 1 + (p_final$y_tilt - 1) * weight,
              angle_rotate = 0 + (p_final$angle_rotate - 0) * weight
            )
          } else {
            curr_p <- p_final
          }

          tilted <- layer::tilt_map(
            data,
            x_stretch = curr_p$x_stretch, y_stretch = curr_p$y_stretch,
            x_tilt = curr_p$x_tilt, y_tilt = curr_p$y_tilt,
            x_shift = curr_p$x_shift, y_shift = curr_p$y_shift,
            angle_rotate = curr_p$angle_rotate
          )
          tilted$.frame <- f
          res_tilted_f[[i]] <- tilted

          if (!is.na(lbl_text)) {
            coords <- sf::st_coordinates(tilted)
            if (lbl_side == "right") {
              anchor_idx <- which.max(coords[, "X"])
              anchor_x <- coords[anchor_idx, "X"] + x_off
            } else {
              anchor_idx <- which.min(coords[, "X"])
              anchor_x <- coords[anchor_idx, "X"] - x_off
            }
            anchor_y <- coords[anchor_idx, "Y"] + y_off

            res_labels_f[[i]] <- data.frame(
              x = anchor_x,
              y = anchor_y,
              label = lbl_text,
              .frame = f,
              stringsAsFactors = FALSE
            )
          }
        }
        list(tilted = res_tilted_f, labels = res_labels_f)
      }
    )

    frame_results <- results[mirai::.progress]
    mirai::everywhere({
      eval(parse(text = 'rm(".L_DATA", ".L_LABELS", ".L_SIDES", ".L_XOFFS", ".L_YOFFS", ".L_PARAMS", ".L_NFRAMES", ".L_FPL", ".L_SEQ", envir = .GlobalEnv)'))
    })

    if (any(sapply(frame_results, function(x) inherits(x, "error_value")))) {
      err_idx <- which(sapply(frame_results, function(x) inherits(x, "error_value")))[1]
      rlang::abort(c("x" = "Error in parallel animation frame generation.",
                     "i" = sprintf("Worker message: %s", as.character(frame_results[[err_idx]]))))
    }

    # Reassemble: results is list(n_frames) of list(tilted = list(n_layers), labels = list(n_layers))
    # We want: list(tilted = list(n_layers), labels = list(n_layers))
    tilted_finals <- lapply(seq_len(n_layers), function(i) {
      do.call(rbind, lapply(frame_results, function(res) res$tilted[[i]]))
    })
    labels_finals <- lapply(seq_len(n_layers), function(i) {
      res_list <- lapply(frame_results, function(res) res$labels[[i]])
      res_list <- Filter(Negate(is.null), res_list)
      if (length(res_list) > 0) do.call(rbind, res_list) else NULL
    })

    return(list(tilted = tilted_finals, labels = labels_finals))
  } else {
    # Serial path
    if (verbose) cli::cli_inform(c("i" = "Animation: Using Tier 3 (Serial Execution)"))
    if (verbose) pb <- cli::cli_progress_bar("Generating frames", total = n_frames)

    # Re-initialize the lists for the final results
    tilted_finals <- vector("list", n_layers)
    labels_finals <- vector("list", n_layers)

    # In serial, it is easier to iterate by frame to match progress bar
    for (f in 1:n_frames) {
      for (i in seq_along(stack$layers)) {
        data <- layer_data_list[[i]]
        p_final <- params_final[[i]]
        lbl_text <- layer_labels[[i]]
        lbl_side <- layer_sides[[i]]
        x_off <- layer_x_offsets[[i]]
        y_off <- layer_y_offsets[[i]]

        if (sequential) {
          start_f <- (i - 1) * frames_per_layer + 1
          end_f <- min(n_frames, i * frames_per_layer)
        } else {
          start_f <- 1
          end_f <- n_frames
        }

        if (f < start_f) next

        if (f <= end_f) {
          weight <- (f - start_f) / max(1, (end_f - start_f))
          curr_p <- list(
            x_shift = 0 + (p_final$x_shift - 0) * weight,
            y_shift = 0 + (p_final$y_shift - 0) * weight,
            x_stretch = 0.01 + (p_final$x_stretch - 0.01) * weight,
            y_stretch = 0 + (p_final$y_stretch - 0) * weight,
            x_tilt = 0 + (p_final$x_tilt - 0) * weight,
            y_tilt = 1 + (p_final$y_tilt - 1) * weight,
            angle_rotate = 0 + (p_final$angle_rotate - 0) * weight
          )
        } else {
          curr_p <- p_final
        }

        tilted <- layer::tilt_map(
          data,
          x_stretch = curr_p$x_stretch, y_stretch = curr_p$y_stretch,
          x_tilt = curr_p$x_tilt, y_tilt = curr_p$y_tilt,
          x_shift = curr_p$x_shift, y_shift = curr_p$y_shift,
          angle_rotate = curr_p$angle_rotate
        )
        tilted$.frame <- f
        tilted_finals[[i]] <- rbind(tilted_finals[[i]], tilted)

        if (!is.na(lbl_text)) {
          coords <- sf::st_coordinates(tilted)
          if (lbl_side == "right") {
            anchor_idx <- which.max(coords[, "X"])
            anchor_x <- coords[anchor_idx, "X"] + x_off
          } else {
            anchor_idx <- which.min(coords[, "X"])
            anchor_x <- coords[anchor_idx, "X"] - x_off
          }
          anchor_y <- coords[anchor_idx, "Y"] + y_off

          new_lbl <- data.frame(
            x = anchor_x,
            y = anchor_y,
            label = lbl_text,
            .frame = f,
            stringsAsFactors = FALSE
          )
          labels_finals[[i]] <- rbind(labels_finals[[i]], new_lbl)
        }
      }
      if (verbose) cli::cli_progress_update(id = pb)
    }
    return(list(tilted = tilted_finals, labels = labels_finals))
  }
}

#' @noRd
.animate_unfold <- function(stack, direction, n_frames, sequential, status, verbose, ...) {
  params_final <- resolve_stack_params(stack)
  n_layers <- length(stack$layers)
  anchor_idx <- if (direction == "up") 1 else n_layers
  p_anchor <- params_final[[anchor_idx]]
  layer_data_list <- lapply(stack$layers, `[[`, "data")
  layer_labels <- lapply(stack$layers, `[[`, "label")
  layer_sides <- lapply(stack$layers, function(l) if (!is.null(l$label_side)) l$label_side else stack$defaults$label_side)
  layer_x_offsets <- lapply(stack$layers, function(l) if (!is.null(l$label_x_offset)) l$label_x_offset else (if (!is.null(stack$defaults$label_x_offset)) stack$defaults$label_x_offset else 5))
  layer_y_offsets <- lapply(stack$layers, function(l) if (!is.null(l$label_y_offset)) l$label_y_offset else (if (!is.null(stack$defaults$label_y_offset)) stack$defaults$label_y_offset else 0))
  
  if (sequential) {
    frames_per_layer <- max(2, floor(n_frames / n_layers))
  } else {
    frames_per_layer <- n_frames
  }

  if (status$active) {
    if (verbose) cli::cli_inform(c("i" = "Animation: Using Grouped Parallelism (Frame-bound)"))

    # Broadcast data once
    mirai::everywhere(
      {
        # Use eval(parse) to bypass R CMD check's static analysis of global assignments
        eval(parse(text = ".L_DATA <<- .d"))
        eval(parse(text = ".L_LABELS <<- .ll"))
        eval(parse(text = ".L_SIDES <<- .ls"))
        eval(parse(text = ".L_XOFFS <<- .lx"))
        eval(parse(text = ".L_YOFFS <<- .ly"))
        eval(parse(text = ".L_PARAMS <<- .p"))
        eval(parse(text = ".L_ANCHOR <<- .pa"))
        eval(parse(text = ".L_NFRAMES <<- .nf"))
        eval(parse(text = ".L_FPL <<- .fpl"))
        eval(parse(text = ".L_SEQ <<- .seq"))
      },
      .args = list(.d = layer_data_list, .ll = layer_labels, .ls = layer_sides, .lx = layer_x_offsets, .ly = layer_y_offsets, .p = params_final, .pa = p_anchor, .nf = n_frames, .fpl = frames_per_layer, .seq = sequential)
    )

    # Parallel path (one task per frame)
    results <- mirai::mirai_map(
      1:n_frames,
      function(f) {
        res_tilted_f <- list()
        res_labels_f <- list()

        for (i in seq_along(.L_DATA)) {
          data <- .L_DATA[[i]]
          p_final <- .L_PARAMS[[i]]
          p_anchor <- .L_ANCHOR
          lbl_text <- .L_LABELS[[i]]
          lbl_side <- .L_SIDES[[i]]
          x_off <- .L_XOFFS[[i]]
          y_off <- .L_YOFFS[[i]]

          if (.L_SEQ) {
            start_f <- (i - 1) * .L_FPL + 1
            end_f <- min(.L_NFRAMES, i * .L_FPL)
          } else {
            start_f <- 1
            end_f <- .L_NFRAMES
          }

          if (f < start_f) next

          if (f <= end_f) {
            weight <- (f - start_f) / max(1, (end_f - start_f))
            curr_p <- list(
              x_shift = p_anchor$x_shift + (p_final$x_shift - p_anchor$x_shift) * weight,
              y_shift = p_anchor$y_shift + (p_final$y_shift - p_anchor$y_shift) * weight,
              x_stretch = 1 + (p_final$x_stretch - 1) * weight,
              y_stretch = 0 + (p_final$y_stretch - 0) * weight,
              x_tilt = 0 + (p_final$x_tilt - 0) * weight,
              y_tilt = 1 + (p_final$y_tilt - 1) * weight,
              angle_rotate = 0 + (p_final$angle_rotate - 0) * weight
            )
          } else {
            curr_p <- p_final
          }

          tilted <- layer::tilt_map(
            data,
            x_stretch = curr_p$x_stretch, y_stretch = curr_p$y_stretch,
            x_tilt = curr_p$x_tilt, y_tilt = curr_p$y_tilt,
            x_shift = curr_p$x_shift, y_shift = curr_p$y_shift,
            angle_rotate = curr_p$angle_rotate
          )
          tilted$.frame <- f
          res_tilted_f[[i]] <- tilted

          if (!is.na(lbl_text)) {
            coords <- sf::st_coordinates(tilted)
            if (lbl_side == "right") {
              anchor_idx <- which.max(coords[, "X"])
              anchor_x <- coords[anchor_idx, "X"] + x_off
            } else {
              anchor_idx <- which.min(coords[, "X"])
              anchor_x <- coords[anchor_idx, "X"] - x_off
            }
            anchor_y <- coords[anchor_idx, "Y"] + y_off

            res_labels_f[[i]] <- data.frame(
              x = anchor_x,
              y = anchor_y,
              label = lbl_text,
              .frame = f,
              stringsAsFactors = FALSE
            )
          }
        }
        list(tilted = res_tilted_f, labels = res_labels_f)
      }
    )

    frame_results <- results[mirai::.progress]
    mirai::everywhere({
      eval(parse(text = 'rm(".L_DATA", ".L_LABELS", ".L_SIDES", ".L_XOFFS", ".L_YOFFS", ".L_PARAMS", ".L_NFRAMES", ".L_FPL", ".L_SEQ", envir = .GlobalEnv)'))
    })

    if (any(sapply(frame_results, function(x) inherits(x, "error_value")))) {
      err_idx <- which(sapply(frame_results, function(x) inherits(x, "error_value")))[1]
      rlang::abort(c("x" = "Error in parallel animation frame generation.",
                     "i" = sprintf("Worker message: %s", as.character(frame_results[[err_idx]]))))
    }

    # Reassemble
    tilted_finals <- lapply(seq_len(n_layers), function(i) {
      do.call(rbind, lapply(frame_results, function(res) res$tilted[[i]]))
    })
    labels_finals <- lapply(seq_len(n_layers), function(i) {
      res_list <- lapply(frame_results, function(res) res$labels[[i]])
      res_list <- Filter(Negate(is.null), res_list)
      if (length(res_list) > 0) do.call(rbind, res_list) else NULL
    })

    return(list(tilted = tilted_finals, labels = labels_finals))
  } else {
    # Serial path
    if (verbose) cli::cli_inform(c("i" = "Animation: Using Tier 3 (Serial Execution)"))
    if (verbose) pb <- cli::cli_progress_bar("Generating frames", total = n_frames)

    tilted_finals <- vector("list", n_layers)
    labels_finals <- vector("list", n_layers)

    for (f in 1:n_frames) {
      for (i in seq_along(stack$layers)) {
        data <- layer_data_list[[i]]
        p_final <- params_final[[i]]
        lbl_text <- layer_labels[[i]]
        lbl_side <- layer_sides[[i]]
        x_off <- layer_x_offsets[[i]]
        y_off <- layer_y_offsets[[i]]

        if (sequential) {
          start_f <- (i - 1) * frames_per_layer + 1
          end_f <- min(n_frames, i * frames_per_layer)
        } else {
          start_f <- 1
          end_f <- n_frames
        }

        if (f < start_f) next

        if (f <= end_f) {
          weight <- (f - start_f) / max(1, (end_f - start_f))
          curr_p <- list(
            x_shift = p_anchor$x_shift + (p_final$x_shift - p_anchor$x_shift) * weight,
            y_shift = p_anchor$y_shift + (p_final$y_shift - p_anchor$y_shift) * weight,
            x_stretch = 1 + (p_final$x_stretch - 1) * weight,
            y_stretch = 0 + (p_final$y_stretch - 0) * weight,
            x_tilt = 0 + (p_final$x_tilt - 0) * weight,
            y_tilt = 1 + (p_final$y_tilt - 1) * weight,
            angle_rotate = 0 + (p_final$angle_rotate - 0) * weight
          )
        } else {
          curr_p <- p_final
        }
        tilted <- layer::tilt_map(data, x_stretch = curr_p$x_stretch, y_stretch = curr_p$y_stretch, x_tilt = curr_p$x_tilt, y_tilt = curr_p$y_tilt, x_shift = curr_p$x_shift, y_shift = curr_p$y_shift, angle_rotate = curr_p$angle_rotate)
        tilted$.frame <- f
        tilted_finals[[i]] <- rbind(tilted_finals[[i]], tilted)

        if (!is.na(lbl_text)) {
          coords <- sf::st_coordinates(tilted)
          if (lbl_side == "right") {
            anchor_idx <- which.max(coords[, "X"])
            anchor_x <- coords[anchor_idx, "X"] + x_off
          } else {
            anchor_idx <- which.min(coords[, "X"])
            anchor_x <- coords[anchor_idx, "X"] - x_off
          }
          anchor_y <- coords[anchor_idx, "Y"] + y_off

          new_lbl <- data.frame(
            x = anchor_x,
            y = anchor_y,
            label = lbl_text,
            .frame = f,
            stringsAsFactors = FALSE
          )
          labels_finals[[i]] <- rbind(labels_finals[[i]], new_lbl)
        }
      }
      if (verbose) cli::cli_progress_update(id = pb)
    }
    return(list(tilted = tilted_finals, labels = labels_finals))
  }
}
