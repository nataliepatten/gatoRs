# Download - Download specimen data from both GBIF and the iDigBio archive

The `gators_download()` function downloads data from GBIF and iDigBio
for your desired species. In June 2026, we modified this function to
queries an archive of iDigBio that contains only recordsets that were
not uploaded to GBIF by June 2026. Since funding ended for iDigBio in
2026, iDigBio no longer has the resources to host data. Though the vast
majority of recordsets migrated to GBIF, we wanted to make sure we had
information available for the remaining few who have not migrated.

## Usage

``` r
gators_download(
  synonyms.list,
  write.file = FALSE,
  filename = NA,
  gbif.match = "fuzzy",
  gbif.prov = FALSE,
  idigbio.match = "fuzzy",
  idigbio.filter = TRUE,
  fuzzy.ratio = 50,
  limit = 1e+05
)
```

## Arguments

- synonyms.list:

  A list of scientific names including the accepted scientific name and
  any synonyms for your desired species. For example,
  `synonyms.list = c("Asclepias curtissii","Asclepias aceratoides", "Asclepias arenicola", "Oxypteryx arenicola", "Oxypteryx curtissii")`.
  This parameter is required.

- write.file:

  A parameter to choose whether to produce a .csv file containing search
  results. This parameter is not required and is assigned FALSE by
  default.

- filename:

  The path and file name for the retrieved data. Note that this
  parameter should include the ".csv" extension as well. For example,
  `filename = "base_folder/other_folder/my_file.csv"`. The file path can
  be entered either as relative to the current working directory
  (example: "../my_file.csv") or as a full path. This parameter is
  required if `write.file = TRUE`.

- gbif.match:

  A parameter to select either search by fuzzy matching of scientific
  name or to search by species code. For example, `gbif.match = "fuzzy"`
  will search by fuzzy match and `gbif.match = "code"` will search by
  code. This parameter is not required and is assigned "fuzzy" by
  default.

- gbif.prov:

  A parameter to obtain the provider/verbatim columns from GBIF. This
  parameter is optional and is assigned `FALSE` by default.

- idigbio.match:

  Default = "fuzzy". Options are "fuzzy" or "exact". The fuzzy match
  uses [rapidfuzz](https://github.com/rapidfuzz/RapidFuzz) to calculate
  a rapidfuzz_partial_token_set_ratio or similarity ratio between the
  two strings. This option is helpful when word order differs or our
  strings are partial matches.

- idigbio.filter:

  A parameter to remove less relevant search results from iDigBio. Based
  on the search input, results may include data points for a different
  species that mention the desired species in the locality information,
  for example. Choosing `idigbio.filter = TRUE` will return the data
  frame with rows in which the name column fuzzy matches a name on the
  synonym list. This parameter is not required and is assigned TRUE by
  default.

- fuzzy.ratio:

  Default = 50%. We let more match than probably are helpful, and return
  all ratios greater than 50%.

- limit:

  Default = 100,000 (maximum). Set limit to the number of records
  requested for each element in synonyms.list from GBIF.

## Value

Returns a data frame and writes a csv file as specified in the input.
This csv file will contain search results for the desired species from
the GBIF and iDigBio databases. The columns are as follows:

- [scientificName](https://dwc.tdwg.org/list/#dwc_scientificName)

- [genus](https://dwc.tdwg.org/list/#dwc_genus)

- [specificEpithet](https://dwc.tdwg.org/list/#dwc_specificEpithet)

- [infraspecificEpithet](https://dwc.tdwg.org/list/#dwc_infraspecificEpithet)

- ID (contains unique IDs defined from GBIF or iDigBio)

- [occurrenceID](https://dwc.tdwg.org/list/#dwc_occurrenceID)

- [basisOfRecord](https://dwc.tdwg.org/list/#dwc_basisOfRecord)

- [eventDate](https://dwc.tdwg.org/list/#dwc_eventDate)

- [year](https://dwc.tdwg.org/list/#dwc_year)

- [month](https://dwc.tdwg.org/list/#dwc_month)

- [day](https://dwc.tdwg.org/list/#dwc_day)

- [institutionCode](https://dwc.tdwg.org/list/#dwc_institutionCode)

- [recordedBy](https://dwc.tdwg.org/list/#dwc_recordedBy)

- [informationWithheld](https://dwc.tdwg.org/list/#dwc_informationWithheld)

- [country](https://dwc.tdwg.org/list/#dwc_country)

- [county](https://dwc.tdwg.org/list/#dwc_county)

- [stateProvince](https://dwc.tdwg.org/list/#dwc_stateProvince)

- [locality](https://dwc.tdwg.org/list/#dwc_locality)

- [latitude](https://dwc.tdwg.org/list/#dwc_decimalLatitude)

- [longitude](https://dwc.tdwg.org/list/#dwc_decimalLongitude)

- [coordinateUncertaintyInMeters](https://dwc.tdwg.org/list/#dwc_coordinateUncertaintyInMeters)

- [habitat](https://dwc.tdwg.org/list/#dwc_habitat)

- aggregator (either GBIF or iDigBio)

## Details

With `gators_download()` you can obtain biodiversity records for your
species of interest from both GBIF and iDigBio. This function is
innovative in how it searches iDigBio. Unlike `spocc::occ()`, we do not
query the iDigBio API using the scientific name field, as this will only
return exact matches. Instead, we designed a “pseudo-fuzzy match” to
search all fields for partial matches to the supplied scientific names.
This function uses the
[`correct_class()`](https://nataliepatten.github.io/gatoRs/reference/correct_class.md),
[`setupduckDB()`](https://nataliepatten.github.io/gatoRs/reference/setupduckDB.md),
[`get_idigbio_arc()`](https://nataliepatten.github.io/gatoRs/reference/get_idigbio_arc.md),
[`get_gbif()`](https://nataliepatten.github.io/gatoRs/reference/get_gbif.md),
[`fix_columns()`](https://nataliepatten.github.io/gatoRs/reference/fix_columns.md),
[`fix_names()`](https://nataliepatten.github.io/gatoRs/reference/fix_names.md),
and
[`filter_fix_names()`](https://nataliepatten.github.io/gatoRs/reference/filter_fix_names.md).
This function requires the packages magrittr, dplyr, duckDB, arrow, DBI,
rgbif, and stringr.

## Examples

``` r
if(exists("crazy")){
  df <- gators_download(synonyms.list = c("Galax urceolata", "Galax aphylla"))
  df <- gators_download(synonyms.list = "Galax urceolata",
                        gbif.match = "code",
                        idigbio.filter = FALSE,
                        limit = 10)
}
```
