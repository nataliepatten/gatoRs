# Full Cleaning - Wrapper function to speed clean

The `full_clean()` function performs automated cleaning steps, including
options for: removing duplicate data points, checking locality
precision, removing points with skewed coordinates, removing plain zero
records, removing records based on basis of record, and spatially
thinning collection points. This function also provides the option to
interactively inspect and remove types of basis of record.

## Usage

``` r
full_clean(
  df,
  synonyms.list,
  event.date = "eventDate",
  year = "year",
  month = "month",
  day = "day",
  occ.id = "occurrenceID",
  remove.NA.occ.id = FALSE,
  remove.NA.date = FALSE,
  aggregator = "aggregator",
  id = "ID",
  taxa.filter = "fuzzy",
  scientific.name = "scientificName",
  accepted.name = NA,
  remove.zero = TRUE,
  precision = TRUE,
  digits = 2,
  remove.skewed = TRUE,
  basis.list = NA,
  basis.of.record = "basisOfRecord",
  latitude = "latitude",
  longitude = "longitude",
  remove.flagged = TRUE,
  thin.points = TRUE,
  distance = 5,
  reps = 100,
  one.point.per.pixel = TRUE,
  raster = NA,
  resolution = 0.5,
  remove.duplicates = TRUE
)
```

## Arguments

- df:

  Data frame of occurrence records.

- synonyms.list:

  A list of synonyms for a species.

- event.date:

  Default = "eventDate". The name of the [event
  date](http://rs.tdwg.org/dwc/terms/eventDate) column in the data
  frame.

- year:

  Default = "year". The name of the
  [year](http://rs.tdwg.org/dwc/terms/year) column in the data frame.

- month:

  Default = "month". The name of the
  [month](http://rs.tdwg.org/dwc/terms/month) column in the data frame.

- day:

  Default = "day". The name of the
  [day](http://rs.tdwg.org/dwc/terms/day) column in the data frame.

- occ.id:

  Default = "occurrenceID". The name of the
  [occurrenceID](http://rs.tdwg.org/dwc/terms/occurrenceID) column in
  the data frame.

- remove.NA.occ.id:

  Default = FALSE. This will remove records with missing occurrence IDs
  when set to `TRUE`.

- remove.NA.date:

  Default = FALSE. This will remove records with missing event dates
  when set to `TRUE`.

- aggregator:

  Default = "aggregator". The name of the column in the data frame that
  identifies the aggregator that provided the record. This is equal to
  iDigBio or GBIF.

- id:

  Default = "ID". The name of the id column in the data frame, which
  contains unique IDs defined from GBIF (keys) or iDigBio (UUID).

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

- remove.zero:

  Default = TRUE. Indicates that points at (0.00, 0.00) should be
  removed.

- precision:

  Default = TRUE. Indicates that coordinates should be rounded to match
  the coordinate uncertainty.

- digits:

  Default = 2. Indicates digits to round coordinates to when
  `precision = TRUE`.

- remove.skewed:

  Default = TRUE. Utilizes the
  [`remove_skewed()`](https://nataliepatten.github.io/gatoRs/reference/remove_skewed.md)
  function to remove skewed coordinate values.

- basis.list:

  A list of basis to keep. If a list is not supplied, this filter will
  not occur.

- basis.of.record:

  Default = "basisOfRecord". The name of the [basis of
  record](http://rs.tdwg.org/dwc/terms/basisOfRecord) column in the data
  frame.

- latitude:

  Default = "latitude". The name of the
  [latitude](http://rs.tdwg.org/dwc/terms/decimalLatitude) column in the
  data frame.

- longitude:

  Default = "longitude". The name of the
  [longitude](http://rs.tdwg.org/dwc/terms/decimalLongitude) column in
  the data frame.

- remove.flagged:

  Default = TRUE. An option to remove points with problematic locality
  information.

- thin.points:

  Default = TRUE. An option to spatially thin occurrence records.

- distance:

  Default = 5. Distance in km to separate records.

- reps:

  Default = 100. Number of times to perform thinning algorithm.

- one.point.per.pixel:

  Default = TRUE. An option to only retain one point per pixel.

- raster:

  Raster object which will be used for ecological niche comparisons. A
  SpatRaster should be used.

- resolution:

  Default = 0.5. Options - 0.5, 2.5, 5, and 10 (in min of a degree). 0.5
  min of a degree is equal to 30 arc sec.

- remove.duplicates:

  Default = TRUE. An option to remove duplicate points.

## Value

df is a data frame with the cleaned data. Information about the columns
in the returned data frame can be found in the documentation for
[`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).
An additional column named "accepted_name" will be returned if an
accepted.name was provided.

## Details

This function is entirely automated and thus does not take advantage of
the interactive options provided in the individual cleaning functions.
Using this wrapper is recommended for data processing that does not
require interactive/manual cleaning and inspection. All cleaning steps,
except taxonomic harmonization, can be bypassed by setting their
associated input variables to FALSE. This function requires packages
dplyr, magrittr, and raster.

## Examples

``` r
cleaned_data <- full_clean(data, synonyms.list = c("Galax urceolata", "Galax aphylla"),
digits = 3, basis.list = c("Preserved Specimen","Physical specimen"),
accepted.name = "Galax urceolata", remove.flagged = FALSE)
```
