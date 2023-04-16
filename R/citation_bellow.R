#' @title Cite Data - Get species, longitude, and latitude columns
#'
#' @description
#' The `citation_bellow` function retrieves and returns the citation information
#' for the data provided by GBIF in a data frame.
#'
#' @details
#' This function requires the rgbif package.
#'
#' @param df Data frame of occurrence records returned from `gators_download()`.
#' @inheritParams correct_class
#'
#' @examples
#' \dontrun{
#' citations <- citation_bellow(data)
#' }
#'
#' @return Returns a list with citation information for the GBIF data downloaded.
#'
#' @importFrom rgbif gbif_citation
#' @export

citation_bellow <- function(df, id = "ID", aggregator = "aggregator") {

  gbif <- df[df[[aggregator]] == "GBIF", ]
  citations <- list()
  for (i in 1:NROW(gbif)) {
    citations[i] <- rgbif::gbif_citation(gbif[[id]][i])$citation$text
  }
  return(citations)
}
