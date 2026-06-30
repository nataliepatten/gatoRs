# Spatial Correction - Spatially thin records

The `thin_points` function returns records based on coordinate thinning
based on a minimum nearest neighbor distance approach.

## Usage

``` r
thin_points(
  df,
  accepted.name = NA,
  distance = 5,
  reps = 100,
  latitude = "latitude",
  longitude = "longitude"
)
```

## Arguments

- df:

  Data frame of occurrence records.

- accepted.name:

  Accepted name of your species. This argument is not required if the
  data frame already contains an accepted_name column.

- distance:

  Default = 5. Distance in km to separate records.

- reps:

  Default = 100. Number of times to perform thinning algorithm.

- latitude:

  Default = "latitude". The name of the
  [latitude](http://rs.tdwg.org/dwc/terms/decimalLatitude) column in the
  data frame.

- longitude:

  Default = "longitude". The name of the
  [longitude](http://rs.tdwg.org/dwc/terms/decimalLongitude) column in
  the data frame.

## Value

df is a data frame with the cleaned data. Information about the columns
in the returned data frame can be found in the documentation for
[`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

## Details

This function is a wrapper for spatial thinning using the spThin package
(Aiello-Lammens et al., 2015) In summary, the thinning algorithm
provided by spThin calculates the pairwise distances between data
points, then randomly samples a single point from all points less than
or equal to the set minimum nearest neighbor distance. This process is
repeated until the pairwise distances among points do not fall below the
minimum nearest neighbor distance.

## Examples

``` r
thinned_data <- thin_points(data, accepted.name = "Galax urceolata")
```
