# Remove Redacted Information - Remove protected or private records prior to publication

The `remove_redacted()` function identifies and removes records where
'aggregator' is not equal to iDigBio or GBIF.

## Usage

``` r
remove_redacted(df, aggregator = "aggregator")
```

## Arguments

- df:

  A data frame downloaded with
  [`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

- aggregator:

  Default = "aggregator". The name of the column in the data frame that
  identifies the aggregator that provided the record. This is equal to
  iDigBio or GBIF.

## Value

A data frame with redacted records removed. Information about the
columns in the returned data frame can be found in the documentation for
[`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

## Details

This function requires no additional packages.

## Examples

``` r
cleaned_data <- remove_redacted(data)
```
