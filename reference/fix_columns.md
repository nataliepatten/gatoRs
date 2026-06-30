# Used in gators_download() - Fill out taxonomic name columns

The `fix_columns()` function fills out the taxonomic name columns based
on available information in the data set. For example, if a row has a
name provided for the scientificName column, this information will be
used to generate the respective genus, specificEpithet, and
infraspecificEpithet columns for that row.

## Usage

``` r
fix_columns(
  df,
  scientific.name = "scientificName",
  genus = "genus",
  species = "specificEpithet",
  infraspecific.epithet = "infraspecificEpithet"
)
```

## Arguments

- df:

  Data frame of occurrence records.

- scientific.name:

  Default = "scientificName". The name of the
  [scientificName](http://rs.tdwg.org/dwc/terms/scientificName) column
  in the data frame.

- genus:

  Default = "genus". The name of the
  [genus](http://rs.tdwg.org/dwc/terms/genus) column in the data frame.

- species:

  Default = "specificEpithet". The name of the
  [specificEpithet](http://rs.tdwg.org/dwc/terms/specificEpithet) column
  in the data frame.

- infraspecific.epithet:

  Default = "infraspecificEpithet". The name of the
  [infraspecificEpithet](http://rs.tdwg.org/dwc/terms/infraspecificEpithet)
  column in the data frame.

## Value

Returns the original data frame with the specified columns.

## Details

This function requires package stringr.

## Examples

``` r
fixed_data <- fix_columns(data)
```
