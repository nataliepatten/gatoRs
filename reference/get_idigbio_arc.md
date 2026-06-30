# Used in gators_download() - Download data from Integrated Digitized Biocollections that are not on GBIF

The `get_idigbio_arc()` function queries an archive of iDigBio that
contains only recordsets that were not uploaded to GBIF by June 2026.
Since funding ended for iDigBio, they are unable to host data. Though
the vast majority of recordsets migrated to GBIF, we wanted to make sure
we had information available for the remaining.

## Usage

``` r
get_idigbio_arc(synonyms.list, idigbio.match = "fuzzy", fuzzy.ratio = 50)
```

## Arguments

- synonyms.list:

  A list of affiliated names for your query.

- idigbio.match:

  Default = "fuzzy". Options are "fuzzy" or "exact". The fuzzy match
  uses [rapidfuzz](https://github.com/rapidfuzz/RapidFuzz) to calculate
  a rapidfuzz_partial_token_set_ratio or similarity ratio between the
  two strings. This option is helpful when word order differs or our
  strings are partial matches.

- fuzzy.ratio:

  Default = 50%. We let more match than probably are helpful, and return
  all ratios greater than 50%.

## Value

A data frame with desired columns from iDigBio.

## Details

This function uses the
[`correct_class()`](https://nataliepatten.github.io/gatoRs/reference/correct_class.md)
function and
[`setupduckDB()`](https://nataliepatten.github.io/gatoRs/reference/setupduckDB.md).
This function requires the packages magrittr, dplyr, duckDB, arrow, and
DBI.

## Examples

``` r
if(exists("crazy")){
df <- get_idigbio_arc("Galax")
}
```
