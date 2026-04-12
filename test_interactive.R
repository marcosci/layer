# Interactive test script using the README example
# Load the package directly from source
devtools::load_all()

# Load required packages
library(sf)
library(ggplot2)

message("--- Running README Example with Connecting Lines ---")

# Load all built-in datasets from the package
data("landscape_1")
data("landscape_2")
data("landscape_3")
data("landscape_points")

# Reduce the number of points so it's not overly cluttered
set.seed(42)
landscape_points <- landscape_points[sample(nrow(landscape_points), 30), ]

# Prep landscape_points so it has a 'value' column as expected by some internals,
# though we'll tell plot_tiltedmaps to use NA for it.
landscape_points <- sf::st_as_sf(data.frame(value = 1:nrow(landscape_points), landscape_points))

# 1. Create the tilted maps as in the README
tilt_landscape_1 <- tilt_map(landscape_1)
tilt_landscape_2 <- tilt_map(landscape_2, x_shift = 25, y_shift = 50)
tilt_landscape_3 <- tilt_map(landscape_3, x_shift = 50, y_shift = 100)

# 2. To get proper 3D occlusion, we need to draw lines in segments between the layers.
#    We create intermediate shifted point layers to match the rasters.
pts_1 <- tilt_map(landscape_points, x_shift = 0, y_shift = 0)
pts_2 <- tilt_map(landscape_points, x_shift = 25, y_shift = 50)
pts_3 <- tilt_map(landscape_points, x_shift = 50, y_shift = 100)
pts_4 <- tilt_map(landscape_points, x_shift = 75, y_shift = 150)

# 3. Create the connecting line segments
lines_1_to_2 <- tilt_lines(pts_1, pts_2)
lines_2_to_3 <- tilt_lines(pts_2, pts_3)
lines_3_to_4 <- tilt_lines(pts_3, pts_4)

# 4. Plot all layers interleaved
# By plotting Layer1 -> Lines1to2 -> Layer2 -> Lines2to3 -> Layer3 -> Lines3to4 -> Points
# the layers will naturally obscure the lines behind them, preserving the 3D illusion!
map_list <- list(
  tilt_landscape_1,
  lines_1_to_2,
  tilt_landscape_2,
  lines_2_to_3,
  tilt_landscape_3,
  lines_3_to_4,
  pts_4
)

plot_readme <- plot_tiltedmaps(
  map_list,
  layer = c("value", NA, "value", NA, "value", NA, NA),
  palette = c("bilbao", NA, "mako", NA, "rocket", NA, NA),
  color = c(NA, "grey60", NA, "grey60", NA, "grey60", "black")
)

print(plot_readme)
message("Plot generated! Check your plot viewer. You should see perfect 3D occlusion.")
