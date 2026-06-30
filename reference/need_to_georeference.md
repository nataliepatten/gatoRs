# Identify Missing Information - Find records which lack coordinate information

The `need_to_georeference()` function allows you to find records that
are missing coordinates but contain locality information. These records
can then be manually georeferenced.

## Usage

``` r
need_to_georeference(
  df,
  longitude = "longitude",
  latitude = "latitude",
  locality = "locality"
)
```

## Arguments

- df:

  A data frame downloaded with
  [`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

- longitude:

  Default = "longitude". The name of the
  [longitude](http://rs.tdwg.org/dwc/terms/decimalLongitude) column in
  the data frame.

- latitude:

  Default = "latitude". The name of the
  [latitude](http://rs.tdwg.org/dwc/terms/decimalLatitude) column in the
  data frame.

- locality:

  Default = "locality". The name of the
  [locality](http://rs.tdwg.org/dwc/terms/locality) column in the data
  frame.

## Value

Returns a data frame of the points that need to be georeferenced.
Information about the columns in the returned data frame can be found in
the documentation for
[`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

## Details

This function requires no additional packages.

## Examples

``` r
need_coords <- need_to_georeference(data)
```
