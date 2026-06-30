# Used in basic_locality_clean() - Removed skewed locality

The `remove_skewed()` function identifies and removes records where
locality has been skewed. Records are considered skewed if
informationWithheld contains the string "Coordinate uncertainty
increased".

## Usage

``` r
remove_skewed(df, info.withheld = "informationWithheld")
```

## Arguments

- df:

  A data frame downloaded with
  [`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

- info.withheld:

  Default = "informationWithheld". The name of the [information
  withheld](http://rs.tdwg.org/dwc/iri/informationWithheld) column in
  the data frame.

## Value

A data frame with records remove only records for which locality was
skewed.

## Details

This function requires no additional packages.

## Examples

``` r
cleaned_data <- remove_skewed(data)
```
