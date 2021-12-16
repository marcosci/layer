
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
#> Loading required package: stars
#> Loading required package: abind
#> Loading required package: sf
#> Linking to GEOS 3.8.1, GDAL 3.2.1, PROJ 7.2.1
#> Registered S3 methods overwritten by 'stars':
#>   method             from
#>   st_bbox.SpatRaster sf  
#>   st_crs.SpatRaster  sf
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: raster
#> Loading required package: sp
#> 
#> Attaching package: 'raster'
#> The following object is masked from 'package:dplyr':
#> 
#>     select
library(ggplot2)
library(scico)

tilt_landscape_1 <- tilt_map(landscape_1)
tilt_landscape_2 <- tilt_map(landscape_2, x_shift = 50, y_shift = 50)
tilt_landscape_3 <- tilt_map(landscape_3, x_shift = 100, y_shift = 100)
tilt_landscape_points <- tilt_map(landscape_points, x_shift = 150, y_shift = 150)

ggplot() +
  geom_sf(data = tilt_landscape_1,
          aes(fill = layer), color = NA) +
  geom_sf(data = tilt_landscape_2,
          aes(fill = layer), color = NA) +
  geom_sf(data = tilt_landscape_3,
          aes(fill = layer), color = NA) +
  geom_sf(data = tilt_landscape_points,
          color = "red") +
  scale_fill_scico(palette = 'davos') +
  theme(legend.position = "none") +
  theme_void()
```

<img src="man/figures/README-example-1.png" width="100%" />
