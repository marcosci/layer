
<!-- README.md is generated from README.Rmd. Please edit that file -->

# layer

<!-- badges: start -->
<!-- badges: end -->

The goal of layer is to …

## Installation

You can install the development version of layer from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("marcosci/layer")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(layer)

tilt_landscape_1 <- tilt_map(landscape_1)
#> Loading required package: raster
#> Loading required package: sp
tilt_landscape_2 <- tilt_map(landscape_2, x_shift = 50, y_shift = 50)
tilt_landscape_3 <- tilt_map(landscape_3, x_shift = 100, y_shift = 100)
tilt_landscape_points <- tilt_map(landscape_points, x_shift = 150, y_shift = 150)

map_list <- list(tilt_landscape_1, tilt_landscape_2, tilt_landscape_3, tilt_landscape_points)

plot_tiltedmaps(map_list, 
                layer = c("value", "value", "value", NA),
                palette = c("bilbao", "mako", "rocket", "turbo"),
                color = "grey40")
#> Warning in if (is.na(layer)) layer <- "value": the condition has length > 1 and
#> only the first element will be used
```

<img src="man/figures/README-example-1.png" width="100%" />

### More advanced examples

Some more realistic looking data (DEM, drought and precipitation for
continental USA):

``` r
tilt_landscape_1 <- tilt_map(dem_usa)
tilt_landscape_2 <- tilt_map(drought_usa, x_shift = 0, y_shift = 25)
tilt_landscape_3 <- tilt_map(prec_usa, x_shift = 0, y_shift = 50)

map_list <- list(tilt_landscape_1, tilt_landscape_2, tilt_landscape_3)

plot_tiltedmaps(map_list, palette =c("tofino", "rocket", "mako"), direction = c(-1, 1, 1)) 
```

<img src="./man/figures/README-figure-real.png" width="100%" />
