#' @title Used in gators_download() - Download data from Integrated Digitized Biocollections that are not on GBIF
#'
#' @description
#' The `get_idigbio_arc()` function queries an archive of iDigBio that contains only
#' recordsets that were not uploaded to GBIF by June 2026. Since funding ended for iDigBio,
#' they are unable to host data. Though the vast majority of recordsets migrated to GBIF,
#' we wanted to make sure we had information available for the remaining.
#'
#' @details
#' This function uses the `correct_class()` function and `setupduckDB()`.
#' This function requires the packages ridigbio, magrittr, dplyr, duckDB, arrow, and DBI.
#'
#' @param synonyms.list A list of affiliated names for your query.
#' @param idigbio.match Default = "fuzzy". Options are "fuzzy" or "exact".
#' The fuzzy match uses [rapidfuzz](https://github.com/rapidfuzz/RapidFuzz) to calculate
#' a rapidfuzz_partial_token_set_ratio or similarity ratio between the two strings. This option
#' is helpful when word order differs or our strings are partial matches.
#' @param fuzzy.ratio Default = 50%. We let more match than probably are helpful, and return all ratios greater than 50%.
#' @examples
#' \dontrun{
#' df <- get_idigbio_arc("Galax")
#'}
#' @return A data frame with desired columns from iDigBio.
#'
#' @importFrom dplyr select distinct
#' @importFrom magrittr "%>%"
#' @importFrom duckdb dbConnect duckdb
#' @importFrom DBI  dbGetQuery
#' @importFrom tools R_user_dir
#' @export


get_idigbio_arc <- function(synonyms.list, idigbio.match = "fuzzy", fuzzy.ratio = 50){
  if (length(synonyms.list) == 0 | any(is.na(synonyms.list))) {
    stop("Invalid argument: synonyms.list. The argument synonyms.list must be non-empty.")
  }

  message(setupduckDB())

  dbdir <- tools::R_user_dir("gatoRs", which = "data")
  dbfile <- file.path(dbdir, "iMFGduckdb.duckdb")
  con <-  duckdb::dbConnect(duckdb::duckdb(), dbdir = dbfile, read_only = TRUE)

  allquery <- c()
  for(x in 1:length(synonyms.list)){
      if(idigbio.match == "fuzzy"){
        allquery[[x]] <- DBI::dbGetQuery(con, paste0("SELECT *,
             rapidfuzz_partial_token_set_ratio(scientificName, '", synonyms.list[x], "') AS match_score
             FROM missing_view
             WHERE rapidfuzz_partial_token_set_ratio(scientificName,'", synonyms.list[x], "') >", fuzzy.ratio))

      }else if(idigbio.match == "exact"){
        allquery[[x]] <- DBI::dbGetQuery(con, paste0("SELECT * FROM missing_view WHERE scientificName = '",synonyms.list[x], "'"))
      }
  }
  df <- do.call(rbind, allquery)
  df <- dplyr::distinct(df)

  df$aggregator <- "iDigBio_archive"
  query_idigbio <- df %>%
    dplyr::select("scientificName",
                  "genus",
                  "specificEpithet",
                  "infraspecificEpithet",
                  "ID",
                  "occurrenceID",
                  "basisOfRecord",
                  "eventDate",
                  "year",
                  "month",
                  "day",
                  "institutionCode",
                  "recordedBy",
                  "country",
                  "county",
                  "stateProvince",
                  "locality",
                  "latitude",
                  "longitude",
                  "coordinateUncertaintyInMeters",
                  "informationWithheld",
                  "habitat",
                  "aggregator") %>%
    suppressWarnings(correct_class())

  query_idigbio$year <- as.character(  query_idigbio$year)
  query_idigbio$month <- as.character(  query_idigbio$month)
  query_idigbio$day <- as.character(  query_idigbio$day)
  query_idigbio$coordinateUncertaintyInMeters <- as.character(  query_idigbio$coordinateUncertaintyInMeters )

  duckdb::dbDisconnect(con)
  rm(con)
  return(query_idigbio)
}
