test_that("tilt_lines connects points correctly", {
  pts <- data.frame(x = c(1, 2), y = c(1, 2), id = c("A", "B"))
  pts <- sf::st_as_sf(pts, coords = c("x", "y"), crs = 4326)
  
  p1 <- pts
  p2 <- pts
  p2$geometry <- p2$geometry + c(10, 10)
  sf::st_crs(p2) <- 4326
  
  lines <- tilt_lines(p1, p2)
  expect_s3_class(lines, "sf")
  expect_equal(nrow(lines), 2)
  expect_true(all(as.character(sf::st_geometry_type(lines)) == "LINESTRING"))
  
  # Check attributes
  expect_equal(lines$id, c("A", "B"))
})

test_that("tilt_lines handles polygons using centroids", {
  p1 <- sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow=TRUE)))
  p2 <- sf::st_polygon(list(matrix(c(2,2, 3,2, 3,3, 2,3, 2,2), ncol=2, byrow=TRUE)))
  
  df1 <- sf::st_sf(id = 1, geometry = sf::st_sfc(p1))
  df2 <- sf::st_sf(id = 1, geometry = sf::st_sfc(p2))
  
  lines <- tilt_lines(df1, df2)
  expect_s3_class(lines, "sf")
  expect_true(all(as.character(sf::st_geometry_type(lines)) == "LINESTRING"))
})

test_that("tilt_lines checks input types and rows", {
  pts1 <- sf::st_as_sf(data.frame(x = 1, y = 1), coords = c("x", "y"))
  pts2 <- sf::st_as_sf(data.frame(x = c(1,2), y = c(1,2)), coords = c("x", "y"))
  
  expect_error(tilt_lines(pts1, pts2), "must have the same number of rows")
  expect_error(tilt_lines(pts1, data.frame(x=1)), "must be `sf` objects")
})