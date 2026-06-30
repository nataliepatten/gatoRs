# Remove Duplicates - Remove records with identical event dates and coordinates

The `remove_duplicates()` function removes records with identical event
dates and occurrence IDs. Prior to utilizing this function, longitude
and latitude columns should be rounded to match the coordinate
uncertainty using the
[`basic_locality_clean()`](https://nataliepatten.github.io/gatoRs/reference/basic_locality_clean.md)
function.

## Usage

``` r
remove_duplicates(
  df,
  event.date = "eventDate",
  aggregator = "aggregator",
  id = "ID",
  occ.id = "occurrenceID",
  year = "year",
  month = "month",
  day = "day",
  latitude = "latitude",
  longitude = "longitude",
  remove.NA.occ.id = FALSE,
  remove.NA.date = FALSE,
  remove.unparseable = FALSE
)
```

## Arguments

- df:

  Data frame of occurrence records returned from
  [`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

- event.date:

  Default = "eventDate". The name of the [event
  date](http://rs.tdwg.org/dwc/terms/eventDate) column in the data
  frame.

- aggregator:

  Default = "aggregator". The name of the column in the data frame that
  identifies the aggregator that provided the record. This is equal to
  iDigBio or GBIF.

- id:

  Default = "ID". The name of the id column in the data frame, which
  contains unique IDs defined from GBIF (keys) or iDigBio (UUID).

- occ.id:

  Default = "occurrenceID". The name of the
  [occurrenceID](http://rs.tdwg.org/dwc/terms/occurrenceID) column in
  the data frame.

- year:

  Default = "year". The name of the
  [year](http://rs.tdwg.org/dwc/terms/year) column in the data frame.

- month:

  Default = "month". The name of the
  [month](http://rs.tdwg.org/dwc/terms/month) column in the data frame.

- day:

  Default = "day". The name of the
  [day](http://rs.tdwg.org/dwc/terms/day) column in the data frame.

- latitude:

  Default = "latitude". The name of the
  [latitude](http://rs.tdwg.org/dwc/terms/decimalLatitude) column in the
  data frame.

- longitude:

  Default = "longitude". The name of the
  [longitude](http://rs.tdwg.org/dwc/terms/decimalLongitude) column in
  the data frame.

- remove.NA.occ.id:

  Default = FALSE. This will remove records with missing occurrence IDs
  when set to `TRUE`.

- remove.NA.date:

  Default = FALSE. This will remove records with missing event dates
  when set to `TRUE`.

- remove.unparseable:

  Default = FALSE. If we cannot parse the event date into individual
  year, month, day categories the user can manually specify. Otherwise,
  if set to TRUE, these rows will simply be removed.

## Value

Return data frame with duplicates removed. Information about the columns
in the returned data frame can be found in the documentation for
[`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

## Details

Here we identify and remove both (1) specimen duplicates and (2)
aggregator duplicates based on each specimens coordinates, occurrenceID,
and eventDate. To leverage all date information available, set
`remove.unparseable = FALSE` to manually populate the year, month, and
day columnsl. Dates are parsed based on ISO 8601 which only includes
time since the Unix epoch, or January 1st, 1970, therefore dates that
occur before 1970 will not be automatically parsed. If we are unable to
parse the included date for particular records, users can choose to
manually enter the year, month, and day for these records when prompted.
If the user chooses to manually enter the event date, the records
eventDate will be printed and the user will be asked to manually enter
the year, month, and day of this eventDate into the console. Users are
only prompted to manually parse event dates for records where year,
month, and day are absent, but eventDate is present and cannot be
parsed. This function also we also confirm all ID (UUID and key) are
unique to remove any within-aggregator duplicates that may accumulate
due to processing errors. This function requires the parsedate and dplyr
packages. Warning, this function will ignore missing occurrence ID and
year, month, day columns if not provided in the data set.

## Examples

``` r
cleaned_data <- remove_duplicates(data)
cleaned_data <- remove_duplicates(data, remove.NA.occ.id = TRUE, remove.NA.date = TRUE)
cleaned_data <- remove_duplicates(data, remove.unparseable = TRUE)
```
