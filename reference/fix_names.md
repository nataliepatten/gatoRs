# Used in gators_download() - Fix taxonomic name capitalization

The `fix_names()` function fixes the capitalization of species names in
the data frame provided to align with accepted capitalization standards.

## Usage

``` r
fix_names(df, scientific.name = "scientificName")
```

## Arguments

- df:

  Data frame with name column to be fixed.

- scientific.name:

  Default = "scientificName". The name of the
  [scientificName](http://rs.tdwg.org/dwc/terms/scientificName) column
  in the data frame.

## Value

Returns df with fixed capitalization in name column.

## Details

This function uses the
[`fixAfterPeriod()`](https://nataliepatten.github.io/gatoRs/reference/fixAfterPeriod.md)
function. This function requires package stringr.

## Examples

``` r
fixed_data <- fix_names(data)
```
