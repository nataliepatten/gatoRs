# Fix taxonomic capitalization of a species name when there are periods involved.

The `fixAfterPeriod()` function fixes taxonomic capitalization of a
string when there are periods in the species name.

## Usage

``` r
fixAfterPeriod(substring)
```

## Arguments

- substring:

  A substring from the name column of the data frame to be fixed.

## Value

Returns the substring with fixed capitalization.

## Details

Requires package stringr.
