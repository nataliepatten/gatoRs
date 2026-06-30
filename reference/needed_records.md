# Identify Missing Information - Find records with redacted or missing data

The `needed_records()` function identifies records with flags, therefore
withheld. This indicates that information is withheld from these records
due to endangered species status, for example. Accessing this
information may require a permit. Or, these records can be removed from
the data set with
[`remove_redacted()`](https://nataliepatten.github.io/gatoRs/reference/remove_redacted.md).

## Usage

``` r
needed_records(df, info.withheld = "informationWithheld")
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

A data frame with only records for which locality was flagged as
redacted or missing. Information about the columns in the returned data
frame can be found in the documentation for
[`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

## Details

This function requires no additional packages.

## Examples

``` r
need_info <- needed_records(data)
```
