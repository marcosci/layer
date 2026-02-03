# Tilt raster and sf data

Tilt and shift maps in any direction.#'

## Usage

``` r
tilt_map(
  data,
  x_stretch = 2,
  y_stretch = 1.2,
  x_tilt = 0,
  y_tilt = 1,
  x_shift = 0,
  y_shift = 0,
  angle_rotate = pi/20,
  boundary = NULL,
  parallel = FALSE
)
```

## Arguments

- data:

  sf or terra/stars/raster object.

- x_stretch:

  Stretch in x dimension. A `numeric` vector of lenght 1.

- y_stretch:

  Stretch in y dimension. A `numeric` vector of lenght 1.

- x_tilt:

  Tilt in x dimension. A `numeric` vector of lenght 1.

- y_tilt:

  Tilt in y dimension. A `numeric` vector of lenght 1.

- x_shift:

  Shift in x dimension. A `numeric` vector of lenght 1.

- y_shift:

  Shift in y dimension. A `numeric` vector of lenght 1.

- angle_rotate:

  Rotation angle.. A `numeric` vector of lenght 1. Default is `pi/20`.

- boundary:

  Another layer that is used to create a boundary that is drawn around
  the data

- parallel:

  `logical` to run in parallel. FALSE (default)

## Value

An `sf` object with tilted and shifted data.

## Details

Code adopted from
https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/geospatial-data/.

## Examples

``` r
tilt_map(landscape_1)
#> Simple feature collection with 10000 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: -31.28689 xmax: 331.7037 ymax: 79.9967
#> CRS:           NA
#> First 10 features:
#>       value                       geometry
#> 1  1.168731 POLYGON ((134.166 79.9967, ...
#> 2  1.168731 POLYGON ((136.1414 79.68383...
#> 3  1.168731 POLYGON ((138.1168 79.37096...
#> 4  1.168731 POLYGON ((140.0922 79.05809...
#> 5  1.168731 POLYGON ((142.0676 78.74522...
#> 6  1.168731 POLYGON ((144.0429 78.43235...
#> 7  1.168731 POLYGON ((146.0183 78.11948...
#> 8  1.242727 POLYGON ((147.9937 77.80662...
#> 9  1.242727 POLYGON ((149.9691 77.49375...
#> 10 1.801846 POLYGON ((151.9444 77.18088...
```
