# gatoRs Download - Correct classes of data frame columns

The `correct_class()` function corrects the classes of each column in a
data frame of your queried species. This function requires no additional
packages.

## Usage

``` r
correct_class(
  df,
  scientific.name = "scientificName",
  genus = "genus",
  species = "specificEpithet",
  infraspecific.epithet = "infraspecificEpithet",
  id = "ID",
  occ.id = "occurrenceID",
  basis.of.record = "basisOfRecord",
  event.date = "eventDate",
  year = "year",
  month = "month",
  day = "day",
  inst.code = "institutionCode",
  recorded.by = "recordedBy",
  country = "country",
  county = "county",
  state = "stateProvince",
  locality = "locality",
  latitude = "latitude",
  longitude = "longitude",
  coord.uncertainty = "coordinateUncertaintyInMeters",
  info.withheld = "informationWithheld",
  habitat = "habitat",
  aggregator = "aggregator"
)
```

## Arguments

- df:

  Data frame returned by `gator_download()`.

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

- id:

  Default = "ID". The name of the id column in the data frame, which
  contains unique IDs defined from GBIF (keys) or iDigBio (UUID).

- occ.id:

  Default = "occurrenceID". The name of the
  [occurrenceID](http://rs.tdwg.org/dwc/terms/occurrenceID) column in
  the data frame.

- basis.of.record:

  Default = "basisOfRecord". The name of the [basis of
  record](http://rs.tdwg.org/dwc/terms/basisOfRecord) column in the data
  frame.

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

- inst.code:

  Default = "institutionCode". The name of the [institution
  code](http://rs.tdwg.org/dwc/terms/institutionCode) column in the data
  frame.

- recorded.by:

  Default = "recordedBy". The
  [recordedBy](http://rs.tdwg.org/dwc/iri/recordedBy) column in the data
  frame.

- country:

  Default = "country". The name of the
  [country](http://rs.tdwg.org/dwc/terms/country) column in the data
  frame.

- county:

  Default = "county". The name of the
  [county](http://rs.tdwg.org/dwc/terms/county) column in the data
  frame.

- state:

  Default = "stateProvince". The name of the
  [state/province](http://rs.tdwg.org/dwc/terms/stateProvince) column in
  the data frame.

- locality:

  Default = "locality". The name of the
  [locality](http://rs.tdwg.org/dwc/terms/locality) column in the data
  frame.

- latitude:

  Default = "latitude". The name of the
  [latitude](http://rs.tdwg.org/dwc/terms/decimalLatitude) column in the
  data frame.

- longitude:

  Default = "longitude". The name of the
  [longitude](http://rs.tdwg.org/dwc/terms/decimalLongitude) column in
  the data frame.

- coord.uncertainty:

  Default = "coordinateUncertaintyInMeters". The name of the [coordinate
  uncertainty](http://rs.tdwg.org/dwc/terms/coordinateUncertaintyInMeters)
  column in the data frame.

- info.withheld:

  Default = "informationWithheld". The name of the [information
  withheld](http://rs.tdwg.org/dwc/iri/informationWithheld) column in
  the data frame.

- habitat:

  Default = "habitat". The name of the
  [habitat](http://rs.tdwg.org/dwc/terms/habitat) column in the data
  frame.

- aggregator:

  Default = "aggregator". The name of the column in the data frame that
  identifies the aggregator that provided the record. This is equal to
  iDigBio or GBIF.

## Value

Returns data frame with corrected classes of each column.

## Details

"aggregator", "infraspecific.epithet", and "recorded.by" columns will be
skipped if they do not exist. This function requires the dplyr package.
