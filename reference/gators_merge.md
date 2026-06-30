# Merge Retained Data - Combined original data set with georeferenced or retained records.

The `gators_merge()` function combines two data sets with identical
column names and returns a single data set.

## Usage

``` r
gators_merge(df1, df2)
```

## Arguments

- df1:

  A data frame downloaded with
  [`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md)
  and prepared using
  [`remove_missing()`](https://nataliepatten.github.io/gatoRs/reference/remove_missing.md).

- df2:

  A data frame with the same columns as df1, but with observations
  generated through georeferencing or through data requests.

## Value

A combined data set.

## Details

Prior to combining a data set with georeferenced or retrieved data,
please use the
[`remove_missing()`](https://nataliepatten.github.io/gatoRs/reference/remove_missing.md)
function to limit duplicate records. This function requires no
additional packages.

## Examples

``` r
removed_missing <- remove_missing(data)
needs_geo <- need_to_georeference(data)
# fill in manually georeferenced data into needs_geo...
merged_data <- gators_merge(removed_missing, needs_geo)
needs_data <- needed_records(data)
# fill in missing information with a data request...
merged_data <- gators_merge(merged_data, needs_data)
```
