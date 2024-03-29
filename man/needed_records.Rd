% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/needed_records.R
\name{needed_records}
\alias{needed_records}
\title{Identify Missing Information - Find records with redacted or missing data}
\usage{
needed_records(df, info.withheld = "informationWithheld")
}
\arguments{
\item{df}{A data frame downloaded with \code{gators_download()}.}

\item{info.withheld}{Default = "informationWithheld". The name of the \href{http://rs.tdwg.org/dwc/iri/informationWithheld}{information withheld} column in the data frame.}
}
\value{
A data frame with only records for which locality was flagged as redacted or missing.
Information about the columns in the returned data frame can be found in the documentation for \code{gators_download()}.
}
\description{
The \code{needed_records()} function identifies records with flags, therefore withheld. This indicates that information
is withheld from these records due to endangered species status, for example. Accessing this information may
require a permit. Or, these records can be removed from the data set with \code{remove_redacted()}.
}
\details{
This function requires no additional packages.
}
\examples{
need_info <- needed_records(data)

}
