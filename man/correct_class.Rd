% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correct_class.R
\name{correct_class}
\alias{correct_class}
\title{gatoRs Download - Correct classes of data frame columns}
\usage{
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
}
\arguments{
\item{df}{Data frame returned by \code{gator_download()}.}

\item{scientific.name}{Default = "scientificName". The name of the \href{http://rs.tdwg.org/dwc/terms/scientificName}{scientificName} column in the data frame.}

\item{genus}{Default = "genus". The name of the \href{http://rs.tdwg.org/dwc/terms/genus}{genus} column in the data frame.}

\item{species}{Default = "specificEpithet". The name of the \href{http://rs.tdwg.org/dwc/terms/specificEpithet}{specificEpithet} column in the data frame.}

\item{infraspecific.epithet}{Default = "infraspecificEpithet". The name of the \href{http://rs.tdwg.org/dwc/terms/infraspecificEpithet}{infraspecificEpithet} column in the data frame.}

\item{id}{Default = "ID". The name of the id column in the data frame, which contains unique IDs defined from GBIF (keys) or iDigBio (UUID).}

\item{occ.id}{Default = "occurrenceID". The name of the \href{http://rs.tdwg.org/dwc/terms/occurrenceID}{occurrenceID} column in the data frame.}

\item{basis.of.record}{Default = "basisOfRecord". The name of the \href{http://rs.tdwg.org/dwc/terms/basisOfRecord}{basis of record} column in the data frame.}

\item{event.date}{Default = "eventDate". The name of the \href{http://rs.tdwg.org/dwc/terms/eventDate}{event date} column in the data frame.}

\item{year}{Default = "year". The name of the \href{http://rs.tdwg.org/dwc/terms/year}{year} column in the data frame.}

\item{month}{Default = "month". The name of the \href{http://rs.tdwg.org/dwc/terms/month}{month} column in the data frame.}

\item{day}{Default = "day". The name of the \href{http://rs.tdwg.org/dwc/terms/day}{day} column in the data frame.}

\item{inst.code}{Default = "institutionCode". The name of the \href{http://rs.tdwg.org/dwc/terms/institutionCode}{institution code} column in the data frame.}

\item{recorded.by}{Default = "recordedBy". The \href{http://rs.tdwg.org/dwc/iri/recordedBy}{recordedBy} column in the data frame.}

\item{country}{Default = "country". The name of the \href{http://rs.tdwg.org/dwc/terms/country}{country} column in the data frame.}

\item{county}{Default = "county". The name of the \href{http://rs.tdwg.org/dwc/terms/county}{county} column in the data frame.}

\item{state}{Default = "stateProvince". The name of the \href{http://rs.tdwg.org/dwc/terms/stateProvince}{state/province} column in the data frame.}

\item{locality}{Default = "locality". The name of the \href{http://rs.tdwg.org/dwc/terms/locality}{locality} column in the data frame.}

\item{latitude}{Default = "latitude". The name of the \href{http://rs.tdwg.org/dwc/terms/decimalLatitude}{latitude} column in the data frame.}

\item{longitude}{Default = "longitude". The name of the \href{http://rs.tdwg.org/dwc/terms/decimalLongitude}{longitude} column in the data frame.}

\item{coord.uncertainty}{Default = "coordinateUncertaintyInMeters". The name of the \href{http://rs.tdwg.org/dwc/terms/coordinateUncertaintyInMeters}{coordinate uncertainty} column in the data frame.}

\item{info.withheld}{Default = "informationWithheld". The name of the \href{http://rs.tdwg.org/dwc/iri/informationWithheld}{information withheld} column in the data frame.}

\item{habitat}{Default = "habitat". The name of the \href{http://rs.tdwg.org/dwc/terms/habitat}{habitat} column in the data frame.}

\item{aggregator}{Default = "aggregator". The name of the column in the data frame that identifies the aggregator
that provided the record. This is equal to iDigBio or GBIF.}
}
\value{
Returns data frame with corrected classes of each column.
}
\description{
The \code{correct_class()} function corrects the classes of each column in a data frame of your queried species.
This function requires no additional packages.
}
\details{
"aggregator", "infraspecific.epithet", and "recorded.by" columns will be skipped if they do not exist.
This function requires the dplyr package.
}
\keyword{internal}
