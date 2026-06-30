# Subset Data - Get species, longitude, and latitude columns

The `data_chomp()` function "chomps" (subsets) a data frame of
occurrence records to only contain the following columns: "species",
"longitude", and "latitude". After using this function data will be
ready for use in
[Maxent](https://biodiversityinformatics.amnh.org/open_source/maxent/),
for example.

## Usage

``` r
data_chomp(
  df,
  accepted.name = NA,
  longitude = "longitude",
  latitude = "latitude"
)
```

## Arguments

- df:

  Data frame of occurrence records returned from
  [`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

- accepted.name:

  The accepted species name for the records.

- longitude:

  Default = "longitude". The name of the
  [longitude](http://rs.tdwg.org/dwc/terms/decimalLongitude) column in
  the data frame.

- latitude:

  Default = "latitude". The name of the
  [latitude](http://rs.tdwg.org/dwc/terms/decimalLatitude) column in the
  data frame.

## Value

Returns data frame with three columns: "species", "longitude", and
"latitude". The "species" column is set by the accepted.name input. This
data frame is ready for downstream applications such as Maxent.

## Details

This function requires the package dplyr.

## Examples

``` r
chomped_data <- data_chomp(data, accepted.name = "Galax urceolata")
```
