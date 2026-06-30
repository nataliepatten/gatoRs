# Spatial Correction - One point per pixel

The `one_point_per_pixel` function retains only one point per raster
pixel. This function is useful when creating present-absent models.

## Usage

``` r
one_point_per_pixel(
  df,
  raster = NA,
  resolution = 0.5,
  precision = TRUE,
  digits = 2,
  longitude = "longitude",
  latitude = "latitude"
)
```

## Arguments

- df:

  Data frame of occurrence records.

- raster:

  Raster object which will be used for ecological niche comparisons. A
  SpatRaster should be used.

- resolution:

  Default = 0.5. Options - 0.5, 2.5, 5, and 10 (in min of a degree). 0.5
  min of a degree is equal to 30 arc sec.

- precision:

  Default = TRUE. Indicates that coordinates should be rounded to match
  the coordinate uncertainty.

- digits:

  Default = 2. Indicates digits to round coordinates to when
  `precision = TRUE`.

- longitude:

  Default = "longitude". The name of the
  [longitude](http://rs.tdwg.org/dwc/terms/decimalLongitude) column in
  the data frame.

- latitude:

  Default = "latitude". The name of the
  [latitude](http://rs.tdwg.org/dwc/terms/decimalLatitude) column in the
  data frame.

## Value

df is a data frame with only one point per pixel. Information about the
columns in the returned data frame can be found in the documentation for
[`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

## Details

This function requires package raster and spatstat.geom.

## Examples

``` r
ready_data <- one_point_per_pixel(data)
```
