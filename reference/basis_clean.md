# Basis Cleaning - Removes records with certain record basis

The `basis_clean()` function removes records based on
[basisOfRecord](http://rs.tdwg.org/dwc/terms/basisOfRecord) column.

## Usage

``` r
basis_clean(df, basis.list = NA, basis.of.record = "basisOfRecord")
```

## Arguments

- df:

  Data frame of occurrence records returned from
  [`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

- basis.list:

  A list of basis to keep. If a list is not supplied, the filter will be
  interactive and users must respond to the function.

- basis.of.record:

  Default = "basisOfRecord". The name of the [basis of
  record](http://rs.tdwg.org/dwc/terms/basisOfRecord) column in the data
  frame.

## Value

Returns a data frame with records of desired record basis. Information
about the columns in the returned data frame can be found in the
documentation for
[`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

## Details

With the interactive method, the function will print all unique
basisOfRecord values in the current data set and then ask the user to
respond in the console to prompts regarding which records, if any,
should be removed based on their basisOfRecord. This function requires
no additional packages.

## Examples

``` r
cleaned_data <- basis_clean(data, basis.list = c("Preserved Specimen","Physical specimen"))
```
