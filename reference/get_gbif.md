# Used in gators_download() - Download data from the Global Biodiversity Information Facility

The `get_gbif()` function queries the Global Biodiversity Information
Facility (GBIF) for your desired species. Limited to 100,000 record
downloads.

## Usage

``` r
get_gbif(synonyms.list, gbif.match = "fuzzy", gbif.prov = FALSE, limit = 1e+05)
```

## Arguments

- synonyms.list:

  A list of affiliated names for your query.

- gbif.match:

  Default = "fuzzy". Either "fuzzy" for fuzzy matching of name or "code"
  to search by species code.

- gbif.prov:

  Default = FALSE. A parameter to obtain the provider/verbatim columns
  from GBIF.

- limit:

  Default = 100,000 (maximum). Set limit to the number of records
  requested for each element in synonyms.list.

## Value

Returns a data frame with desired columns from GBIF.

## Details

This function uses the
[`correct_class()`](https://nataliepatten.github.io/gatoRs/reference/correct_class.md)
function. This function requires the packages rgbif, magrittr, and
dplyr.

## Examples

``` r
df <- get_gbif(c("Galax urceolata", "Galax aphylla"), limit = 5)
df <- get_gbif(c("Galax urceolata", "Galax aphylla"),
gbif.match = "code", limit = 5)
df <- get_gbif(c("Galax urceolata", "Galax aphylla"), gbif.prov = TRUE, limit = 5)
```
