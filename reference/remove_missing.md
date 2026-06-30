# Remove Missing Information - Prepare to merge a data frame with georeferenced and retrieved records

The `remove_missing()` function identifies and removes records
identified with the
[`need_to_georeference()`](https://nataliepatten.github.io/gatoRs/reference/need_to_georeference.md)
and
[`needed_records()`](https://nataliepatten.github.io/gatoRs/reference/needed_records.md)
functions. This function should be utilized prior to merging
georeferenced or retrieved records.

## Usage

``` r
remove_missing(
  df,
  remove.type = "both",
  info.withheld = "informationWithheld",
  longitude = "longitude",
  latitude = "latitude",
  locality = "locality",
  id = "ID"
)
```

## Arguments

- df:

  A data frame downloaded with
  [`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

- remove.type:

  Default equal to "both" indicating records identified with the
  [`need_to_georeference()`](https://nataliepatten.github.io/gatoRs/reference/need_to_georeference.md)
  function and
  [`needed_records()`](https://nataliepatten.github.io/gatoRs/reference/needed_records.md)
  function are removed from the data frame. If equal to "georeference"
  then only records identified by the
  [`need_to_georeference()`](https://nataliepatten.github.io/gatoRs/reference/need_to_georeference.md)
  function are removed. If equal to "withheld" then only records
  identified with the
  [`needed_records()`](https://nataliepatten.github.io/gatoRs/reference/needed_records.md)
  function are removed.

- info.withheld:

  Default = "informationWithheld". The name of the [information
  withheld](http://rs.tdwg.org/dwc/iri/informationWithheld) column in
  the data frame.

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

- id:

  Default = "ID". The name of the id column in the data frame, which
  contains unique IDs defined from GBIF (keys) or iDigBio (UUID).

## Value

A data frame with records containing missing information removed.
Information about the columns in the returned data frame can be found in
the documentation for
[`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

## Details

This function requires no additional packages.

## Examples

``` r
cleaned_data <- remove_missing(data)
```
