# Locality Cleaning - Find possibly problematic occurrence records

The `process_flagged()` function allows you to visualize and inspect
possible problematic points, as well as manually remove these points, if
desired. By default, this function is interactive. When running the
function interactively you can hover over a point to see the record's
scientific name, and click on a point to see the record's
coordinates.The interactive option plots flagged points in red and
non-flagged points in blue.

## Usage

``` r
process_flagged(
  df,
  interactive = TRUE,
  latitude = "latitude",
  longitude = "longitude",
  scientific.name = "scientificName"
)
```

## Arguments

- df:

  Data frame of occurrence records returned from
  [`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

- interactive:

  Default = TRUE. The interactive option allows for a visual display of
  possible problematic points and the ability to manually remove these
  points. Setting `interactive = FALSE` will automatically remove these
  points from the data frame.

- latitude:

  Default = "latitude". The name of the
  [latitude](http://rs.tdwg.org/dwc/terms/decimalLatitude) column in the
  data frame.

- longitude:

  Default = "longitude". The name of the
  [longitude](http://rs.tdwg.org/dwc/terms/decimalLongitude) column in
  the data frame.

- scientific.name:

  Default = "scientificName". The name of the
  [scientificName](http://rs.tdwg.org/dwc/terms/scientificName) column
  in the data frame.

## Value

Return cleaned data frame. Information about the columns in the returned
data frame can be found in the documentation for
[`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

## Details

This function is a wrapper to visualize results for the
[`CoordinateCleaner::clean_coordinates()`](https://ropensci.github.io/CoordinateCleaner/reference/clean_coordinates.html)
function. Briefly,
[`CoordinateCleaner::clean_coordinates()`](https://ropensci.github.io/CoordinateCleaner/reference/clean_coordinates.html)
flags records with coordinates that are unlikely valid, spatial
outliers, or in certain locations including the ocean, state capitals,
country centroids, the GBIF headquarters, and biodiversity institutions
(including botanical gardens, museums, herbaria, etc.). This function
requires packages CoordinateCleaner, leaflet, and magrittr. This
function requires interactive user input.

## Examples

``` r
# \donttest{
cleaned_data <- process_flagged(data, interactive = FALSE)
#> Testing coordinate validity
#> Flagged 0 records.
#> Testing equal lat/lon
#> Flagged 0 records.
#> Testing zero coordinates
#> Flagged 0 records.
#> Testing country capitals
#> Flagged 0 records.
#> Testing country centroids
#> Flagged 0 records.
#> Testing sea coordinates
#> Reading ne_50m_land.zip from naturalearth...
#> Flagged 0 records.
#> Testing geographic outliers
#> Testing GBIF headquarters, flagging records around Copenhagen
#> Flagged 0 records.
#> Testing biodiversity institutions
#> Flagged 0 records.
#> Flagged 0 of 8 records, EQ = 0.
# }
```
