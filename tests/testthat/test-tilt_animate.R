test_that("tilt_match works correctly", {
  stack <- tilt_stack(x_shift_step = 10, y_shift_step = 20) |>
    tilt_layer(landscape_1) |>
    tilt_layer(landscape_2)
  
  # Match layer 2
  matched <- tilt_match(landscape_points, stack, layer = 2)
  
  # Manual calculation for verification
  params <- resolve_stack_params(stack)
  p2 <- params[[2]]
  manual <- tilt_map(landscape_points, 
                     x_stretch = p2$x_stretch, y_stretch = p2$y_stretch, 
                     x_tilt = p2$x_tilt, y_tilt = p2$y_tilt, 
                     x_shift = p2$x_shift, y_shift = p2$y_shift, 
                     angle_rotate = p2$angle_rotate)
  
  expect_equal(sf::st_geometry(matched), sf::st_geometry(manual))
})

test_that("tilt_match handles out of bounds errors", {
  stack <- tilt_stack() |> tilt_layer(landscape_1)
  expect_error(tilt_match(landscape_1, stack, layer = 0))
  expect_error(tilt_match(landscape_1, stack, layer = 2))
})

test_that("animate_tilt_stack returns gganim object", {
  skip_if_not_installed("gganimate")
  
  stack <- tilt_stack() |> 
    tilt_layer(landscape_1) |> 
    tilt_layer(landscape_2)
  
  anim_unfold <- animate_tilt_stack(stack, type = "unfold", n_frames = 5)
  expect_s3_class(anim_unfold, "gganim")
  
  anim_reveal <- animate_tilt_stack(stack, type = "reveal", n_frames = 5)
  expect_s3_class(anim_reveal, "gganim")
})
