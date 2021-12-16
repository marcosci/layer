## code to prepare `DATASET` dataset goes here
library(NLMR)
library(sf)

landscape_1 <- nlm_randomrectangularcluster(100, 100,
  minl = 5,
  maxl = 10
) + 1

landscape_2 <- nlm_fbm(100, 100) + 1

landscape_3 <- nlm_distancegradient(100, 100, origin = c(1, 1, 1, 1)) + 1

# get parameters
res <- raster::res(landscape_3)

# random points in the center of the cells
ptscell <- sample(1:length(landscape_3), 500, prob = landscape_3[], replace = TRUE)

# get the centers
center <- raster::xyFromCell(landscape_3, ptscell)

# add random values within the pixels
pts <- center + cbind(
  runif(nrow(center), -res[1] / 2, res[1] / 2),
  runif(nrow(center), -res[2] / 2, res[2] / 2)
)

landscape_points <- pts %>%
  as.data.frame() |>
  sf::st_as_sf(coords = c(1, 2))


usethis::use_data(landscape_1, overwrite = TRUE)
usethis::use_data(landscape_2, overwrite = TRUE)
usethis::use_data(landscape_3, overwrite = TRUE)
usethis::use_data(landscape_points, overwrite = TRUE)
