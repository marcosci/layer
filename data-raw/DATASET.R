## code to prepare `DATASET` dataset goes here
library(NLMR)
library(sf)

landscape_1 <- nlm_curds(curds = c(0.5, 0.3, 0.6),
                         recursion_steps = c(32, 6, 2))

landscape_2 <- nlm_fbm(384, 384)

landscape_3 <- nlm_distancegradient(100, 100, origin = c(1, 1, 1, 1))

# get parameters
res = raster::res(base_raster)

# random points in the center of the cells
ptscell <- sample(1:length(base_raster), 500, prob = base_raster[], replace = TRUE)

# get the centers
center <- raster::xyFromCell(base_raster, ptscell)

# add random values within the pixels
pts <- center + cbind(runif(nrow(center), - res[1]/2, res[1]/2),
                      runif(nrow(center), - res[2]/2, res[2]/2))

landscape_points <- st_multipoint(x = as.matrix(pts), dim = "XY")

usethis::use_data(landscape_1, overwrite = TRUE)
usethis::use_data(landscape_2, overwrite = TRUE)
usethis::use_data(landscape_3, overwrite = TRUE)
usethis::use_data(landscape_points, overwrite = TRUE)

