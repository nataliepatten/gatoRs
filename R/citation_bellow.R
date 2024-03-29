#' @title Cite Data - Get GBIF citations
#'
#' @description
#' The `citation_bellow` function retrieves and returns the citation information
#' for records from GBIF, or where aggregator = "GBIF".
#'
#' @details
#' This function requires the rgbif package.
#'
#' @param df Data frame of occurrence records returned from `gators_download()`.
#' @inheritParams correct_class
#'
#' @examples
#' \donttest{
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
    citations[[i]] <- rgbif::gbif_citation(gbif[[id]][i])$citation$text
  }
  citations <- do.call(rbind, citations)
  return(citations)
}
