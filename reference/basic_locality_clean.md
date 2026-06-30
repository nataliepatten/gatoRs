# Locality Cleaning - Remove missing and improbable coordinates

The `basic_locality_clean()` function cleans locality by removing
missing or impossible coordinates and correcting precision. This
function requires columns named 'latitude' and 'longitude'. These
columns should be of type 'numeric'.

## Usage

``` r
basic_locality_clean(
  df,
  latitude = "latitude",
  longitude = "longitude",
  remove.zero = TRUE,
  precision = TRUE,
  digits = 2,
  remove.skewed = TRUE,
  info.withheld = "informationWithheld"
)
```

## Arguments

- df:

  Data frame of occurrence records returned from
  [`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

- latitude:

  Default = "latitude". The name of the
  [latitude](http://rs.tdwg.org/dwc/terms/decimalLatitude) column in the
  data frame.

- longitude:

  Default = "longitude". The name of the
  [longitude](http://rs.tdwg.org/dwc/terms/decimalLongitude) column in
  the data frame.

- remove.zero:

  Default = TRUE. Indicates that points at (0.00, 0.00) should be
  removed.

- precision:

  Default = TRUE. Indicates that coordinates should be rounded to match
  the coordinate uncertainty.

- digits:

  Default = 2. Indicates digits to round coordinates to when
  `precision = TRUE`.

- remove.skewed:

  Default = TRUE. Utilizes the
  [`remove_skewed()`](https://nataliepatten.github.io/gatoRs/reference/remove_skewed.md)
  function to remove skewed coordinate values.

- info.withheld:

  Default = "informationWithheld". The name of the [information
  withheld](http://rs.tdwg.org/dwc/iri/informationWithheld) column in
  the data frame.

## Value

Return data frame with specimen removed that had missing or improper
coordinate values. Information about the columns in the returned data
frame can be found in the documentation for
[`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

## Details

This function removes any records with missing coordinates, impossible
coordinates, coordinates at (0,0), and any that are flagged as skewed.
These skewed records are identified with the
[`remove_skewed()`](https://nataliepatten.github.io/gatoRs/reference/remove_skewed.md)
function which identifies rows where the
[‘InformationWitheld’](http://rs.tdwg.org/dwc/terms/informationWithheld)
column includes the string "Coordinate uncertainty increased". We also
provide the option to round the provided latitude and longitude values
to a specified number of decimal places. This function requires no
additional packages.

## Examples

``` r
cleaned_data <- basic_locality_clean(data)
```
