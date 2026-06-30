# Used in gators_download() - Filter iDigBio results by scientific name

The `filter_fix_names()` function filters a data frame for relevant
results, based on the scientific name given. Some downloaded results
from iDigBio might contain occurrences of other species that have
"notes" or "locality" strings that mentioning the desired species. Here
we only retain those where the scientificName column is found to be a
fuzzy match to a value in the user-provided list containing the
scientific name and applicable synonym Hence, this function looks for
relevant results that are actually occurrences of the desired species.

## Usage

``` r
filter_fix_names(
  df,
  synonyms.list,
  filter = "fuzzy",
  scientific.name = "scientificName",
  accepted.name = NA
)
```

## Arguments

- df:

  Data frame with name column to be fixed.

- synonyms.list:

  A list of synonyms for a species.

- filter:

  Default = "fuzzy". Indicates the type of filter to be used–either
  "exact" or "fuzzy".

- scientific.name:

  Default = "scientificName". The name of the
  [scientificName](http://rs.tdwg.org/dwc/terms/scientificName) column
  in the data frame.

- accepted.name:

  The accepted scientific name for the species. If provided, an
  additional column will be added to the data frame with the accepted
  name for further manual comparison.

## Value

Returns data frame with filtered results.

## Details

This function requires no additional packages.

## Examples

``` r
cleaned_data <- filter_fix_names(data, c("Galax urceolata", "Galax aphylla"), filter = "exact")
cleaned_data <- filter_fix_names(data, c("Galax urceolata", "Galax aphylla"),
accepted.name = "Galax urceolata")
```
