# Tilt raster and sf data

Takes tilted maps and plots them with ggplot.

## Usage

``` r
plot_tiltedmaps(
  map_list,
  layer = NA,
  palette = "viridis",
  color = "grey50",
  direction = 1,
  begin = 0,
  end = 1,
  alpha = 1
)
```

## Arguments

- map_list:

  sf or terra/stars/raster object.

- layer:

  vector or list of names of each column in tilted sf object that should
  be used for coloring

- palette:

  vector of palettes provided by the viridis and
  [scico](https://rdrr.io/pkg/scico/man/scico.html) packages for rasters

- color:

  a single color applied multiple times or a vector of color strings for
  points or linestrings

- direction:

  vector of directions for viridis and
  [scico](https://rdrr.io/pkg/scico/man/scico.html) color palettes

- begin:

  vector of the of the start of interval the palette to sample colours
  from for viridis and [scico](https://rdrr.io/pkg/scico/man/scico.html)
  color palettes

- end:

  vector of the of the end of interval the palette to sample colours
  from for viridis and [scico](https://rdrr.io/pkg/scico/man/scico.html)
  color palettes

- alpha:

  vector of opacity for viridis and
  [scico](https://rdrr.io/pkg/scico/man/scico.html) color palettes

## Value

A `ggplot` object with stacked maps.

## Examples

``` r
# \donttest{
# tilt data
tilt_landscape_1 <- tilt_map(landscape_1)
tilt_landscape_2 <- tilt_map(landscape_2, x_shift = 50, y_shift = 50)

# plot
map_list <- list(tilt_landscape_1, tilt_landscape_2)
plot_tiltedmaps(map_list, palette = "turbo")

# }
```
