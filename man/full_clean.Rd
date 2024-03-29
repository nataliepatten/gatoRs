% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/full_clean.R
\name{full_clean}
\alias{full_clean}
\title{Full Cleaning - Wrapper function to speed clean}
\usage{
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
}
\arguments{
\item{df}{Data frame of occurrence records.}

\item{synonyms.list}{A list of synonyms for a species.}

\item{event.date}{Default = "eventDate". The name of the \href{http://rs.tdwg.org/dwc/terms/eventDate}{event date} column in the data frame.}

\item{year}{Default = "year". The name of the \href{http://rs.tdwg.org/dwc/terms/year}{year} column in the data frame.}

\item{month}{Default = "month". The name of the \href{http://rs.tdwg.org/dwc/terms/month}{month} column in the data frame.}

\item{day}{Default = "day". The name of the \href{http://rs.tdwg.org/dwc/terms/day}{day} column in the data frame.}

\item{occ.id}{Default = "occurrenceID". The name of the \href{http://rs.tdwg.org/dwc/terms/occurrenceID}{occurrenceID} column in the data frame.}

\item{remove.NA.occ.id}{Default = FALSE. This will remove records with missing occurrence IDs when set to \code{TRUE}.}

\item{remove.NA.date}{Default = FALSE. This will remove records with missing event dates when set to \code{TRUE}.}

\item{aggregator}{Default = "aggregator". The name of the column in the data frame that identifies the aggregator
that provided the record. This is equal to iDigBio or GBIF.}

\item{id}{Default = "ID". The name of the id column in the data frame, which contains unique IDs defined from GBIF (keys) or iDigBio (UUID).}

\item{taxa.filter}{The type of filter to be used--either "exact", "fuzzy", or "interactive".}

\item{scientific.name}{Default = "scientificName". The name of the \href{http://rs.tdwg.org/dwc/terms/scientificName}{scientificName} column in the data frame.}

\item{accepted.name}{The accepted scientific name for the species. If provided, an additional column will be added to the data frame with the accepted name for further manual comparison.}

\item{remove.zero}{Default = TRUE. Indicates that points at (0.00, 0.00) should be removed.}

\item{precision}{Default = TRUE. Indicates that coordinates should be rounded to match the coordinate uncertainty.}

\item{digits}{Default = 2. Indicates digits to round coordinates to when \code{precision = TRUE}.}

\item{remove.skewed}{Default = TRUE. Utilizes the \code{remove_skewed()} function to remove skewed coordinate values.}

\item{basis.list}{A list of basis to keep. If a list is not supplied, this filter will not occur.}

\item{basis.of.record}{Default = "basisOfRecord". The name of the \href{http://rs.tdwg.org/dwc/terms/basisOfRecord}{basis of record} column in the data frame.}

\item{latitude}{Default = "latitude". The name of the \href{http://rs.tdwg.org/dwc/terms/decimalLatitude}{latitude} column in the data frame.}

\item{longitude}{Default = "longitude". The name of the \href{http://rs.tdwg.org/dwc/terms/decimalLongitude}{longitude} column in the data frame.}

\item{remove.flagged}{Default = TRUE. An option to remove points with problematic locality information.}

\item{thin.points}{Default = TRUE. An option to spatially thin occurrence records.}

\item{distance}{Default = 5. Distance in km to separate records.}

\item{reps}{Default = 100. Number of times to perform thinning algorithm.}

\item{one.point.per.pixel}{Default = TRUE. An option to only retain one point per pixel.}

\item{raster}{Raster object which will be used for ecological niche comparisons. A SpatRaster should be used.}

\item{resolution}{Default = 0.5. Options - 0.5, 2.5, 5, and 10 (in min of a degree). 0.5 min of a degree is equal to 30 arc sec.}

\item{remove.duplicates}{Default = TRUE. An option to remove duplicate points.}
}
\value{
df is a data frame with the cleaned data.
Information about the columns in the returned data frame can be found in the documentation for \code{gators_download()}. An additional column named "accepted_name" will be returned if an accepted.name was provided.
}
\description{
The \code{full_clean()} function performs automated cleaning steps, including options for: removing
duplicate data points, checking locality precision, removing points with skewed coordinates,
removing plain zero records, removing records based on basis of record, and spatially thinning collection points.
This function also provides the option to interactively inspect and remove types of basis of record.
}
\details{
This function is entirely automated and thus does not take advantage of the interactive options provided in the individual cleaning functions.
Using this wrapper is recommended for data processing that does not require interactive/manual cleaning and inspection.
All cleaning steps, except taxonomic harmonization, can be bypassed by setting their associated input variables to FALSE.
This function requires packages dplyr, magrittr, and raster.
}
\examples{
cleaned_data <- full_clean(data, synonyms.list = c("Galax urceolata", "Galax aphylla"),
digits = 3, basis.list = c("Preserved Specimen","Physical specimen"),
accepted.name = "Galax urceolata", remove.flagged = FALSE)

}
