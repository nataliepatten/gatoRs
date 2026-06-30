# Taxonomic Cleaning - Filter and resolve taxon names

The `taxa_clean()` function filters a data frame for relevant results,
based on the scientific name given. Filtering can be done with scripts
by exact or fuzzy match. Or, for a more controlled approach, this
function provides interactive filtering by providing the user with
prompts. The interactive method allows the user to manually determine
whether they wish to keep results containing certain scientific names.

## Usage

``` r
taxa_clean(
  df,
  synonyms.list,
  taxa.filter = "fuzzy",
  scientific.name = "scientificName",
  accepted.name = NA
)
```

## Arguments

- df:

  Data frame of occurrence records returned from
  [`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

- synonyms.list:

  A list of synonyms for a species.

- taxa.filter:

  The type of filter to be used–either "exact", "fuzzy", or
  "interactive".

- scientific.name:

  Default = "scientificName". The name of the
  [scientificName](http://rs.tdwg.org/dwc/terms/scientificName) column
  in the data frame.

- accepted.name:

  The accepted scientific name for the species. If provided, an
  additional column will be added to the data frame with the accepted
  name for further manual comparison.

## Value

Returns data frame with filtered results and new column with the
accepted name labeled as "accepted_name". Information about the columns
in the returned data frame can be found in the documentation for
[`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).
An additional column named "accepted_name" will be returned if an
accepted.name was provided.

## Details

If users select the interactive approach, the function will first print
all unique scientific names in the current data set and then ask the
user to respond in the console to prompts regarding which records, if
any, should be removed based on their scientific name. After filtering,
based on a user-provided taxonomy, an accepted name column can be
defined with an optional argument. This function relies on the
user-provided taxonomy, we do not utilize any taxonomic backbone.
Additionally, this function requires no additional packages.

## Examples

``` r
cleaned_data <- taxa_clean(data, c("Galax urceolata", "Galax aphylla"), taxa.filter = "exact")
#> Current scientific names: 
#> [1] "Galax urceolata (Poir.) Brummitt" "Galax aphylla L."                
#> [3] "Galax urceolata"                  "Galax aphylla"                   
#> User selected a(n) exact match.
cleaned_data <- taxa_clean(data, c("Galax urceolata", "Galax aphylla"),
accepted.name = "Galax urceolata")
#> Current scientific names: 
#> [1] "Galax urceolata (Poir.) Brummitt" "Galax aphylla L."                
#> [3] "Galax urceolata"                  "Galax aphylla"                   
#> User selected a(n) fuzzy match.
```
