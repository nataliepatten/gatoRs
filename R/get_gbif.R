#' @title Used in gators_download() - Download data from the Global Biodiversity Information Facility
#'
#' @description
#' The `get_gbif()` function queries the Global Biodiversity Information Facility (GBIF) for your desired species.
#' Limited to 100,000 record downloads.
#'
#' @details
#' This function uses the `correct_class()` function.
#' This function requires the packages rgbif, magrittr, and dplyr.
#'
#' @param synonyms.list A list of affiliated names for your query.
#' @param gbif.match Default = "fuzzy". Either "fuzzy" for fuzzy matching of name or "code" to search by species code.
#' @param limit Default = 100,000 (maximum). Set limit to the number of records requested for each element in synonyms.list.
#'
#' @examples
#' df <- get_gbif(c("Galax urceolata", "Galax aphylla"), limit = 1000)
#' df <- get_gbif(c("Galax urceolata", "Galax aphylla"), gbif.match = "code", limit = 1000)
#'
#' @return Returns a data frame with desired columns from GBIF.
#'
#' @importFrom dplyr bind_rows rename select
#' @importFrom rgbif occ_data name_backbone
#' @importFrom magrittr "%>%"
#'
#' @export


get_gbif <- function(synonyms.list, gbif.match = "fuzzy", limit = 100000){
  if (gbif.match != "fuzzy" & gbif.match != "code") {
    stop("Invalid value for argument: gbif.match. Value for gbif.match must equal 'fuzzy' or 'code'.")
  }
  if (length(synonyms.list) == 0 | any(is.na(synonyms.list))) {
    stop("Invalid argument: synonyms.list. The argument synonyms.list must be non-empty.")
  }

  colNames <- c("scientificName",
                "genus",
                "specificEpithet",
                "infraspecificEpithet",
                "key",
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
                "occurrenceRemarks",
                "verbatimLocality",
                "decimalLatitude",
                "verbatimLatitude",
                "decimalLongitude",
                "verbatimLongitude",
                "coordinateUncertaintyInMeters",
                "informationWithheld",
                "habitat",
                "geodeticDatum")
  numerical <- c("decimalLatitude",
                 "verbatimLatitude",
                 "decimalLongitude",
                 "verbatimLongitude",
                 "year",
                 "month",
                 "day",
                 "coordinateUncertaintyInMeters")

  query_gbif <- data.frame(matrix(ncol = length(colNames), nrow = 0))
  colnames(query_gbif) <- colNames
  # fix columns to be of type character
  for (i in 1:NCOL(query_gbif)) {
    if (colNames[i] %in% numerical) {
      query_gbif[,i] <- as.numeric(query_gbif[,i])
    }
    else {
      query_gbif[,i] <- as.character(query_gbif[,i])
    }
  }

  if (gbif.match == "code") {
    for (i in 1:length(synonyms.list)) {
      key <- suppressWarnings(rgbif::name_backbone(name = synonyms.list[i])$speciesKey)
      if (!is.null(key)) {
        temp <- rgbif::occ_data(taxonKey = key, limit = limit, curlopts=list(http_version=2))
        temp <- temp$data
        query_gbif <- dplyr::bind_rows(query_gbif, temp)
      }
    }
  }
  else if (gbif.match == "fuzzy") {
    for (i in 1:length(synonyms.list)) {
      temp <- rgbif::occ_data(scientificName = synonyms.list[i], limit = limit, curlopts=list(http_version=2))
      temp <- temp$data
      # use bind_rows() to account for different number of columns
      query_gbif <- dplyr::bind_rows(query_gbif, temp)
    }
  }

  # if no results found
  if (NROW(query_gbif) == 0) {
    message("No results found in GBIF.")
    return(query_gbif)
  }

  temp <- data.frame(matrix(NA, ncol = 0, nrow = 0))
  tempColNames <- colnames(temp)

  for (i in 1:length(colNames)) {
    if (!(colNames[i] %in% colnames(query_gbif))) {
      temp <- data.frame(matrix(NA, ncol = NCOL(temp) + 1, nrow = 0))
      colnames(temp) <- c(tempColNames, colNames[i])
      tempColNames <- colnames(temp)
     }
  }

  if (NCOL(temp) > 0) {
    for (i in 1:NCOL(temp)) {
      if (colNames[i] %in% numerical) {
        temp[,i] <- as.numeric(temp[,i])
      }
      else {
        temp[,i] <- as.character(temp[,i])
      }
    }
  }

  query_gbif <- dplyr::bind_rows(temp, query_gbif)
  query_gbif <- dplyr::rename(query_gbif, ID = "key",
                              latitude = "decimalLatitude",
                              verbatimLatitude = "verbatimLatitude",
                              longitude = "decimalLongitude",
                              verbatimLongitude = "verbatimLongitude")
  query_gbif <- suppressWarnings(correct_class(query_gbif))

  # Add occurrenceRemarks, verbatimLocality to locality column
  query_gbif$locality <- paste("locality: ", query_gbif$locality)
  query_gbif$locality <- paste(query_gbif$locality, query_gbif$occurrenceRemarks, sep = ", occurrenceRemarks: ")
  query_gbif$locality <- paste(query_gbif$locality, query_gbif$verbatimLocality, sep = ", verbatimLocality: ")

  # if decimal lat/lon columns are empty, replace with verbatim lat/lon columns
  temp <- query_gbif[is.na(query_gbif$latitude), ]
  query_gbif <- query_gbif[!(is.na(query_gbif$latitude)), ]
  for (i in 1:NROW(temp)) {
    if (!is.na(temp$verbatimLatitude[i]))
      temp$latitude[i] <- temp$verbatimLatitude[i]
  }
  query_gbif <- rbind(query_gbif, temp)

  temp <- query_gbif[is.na(query_gbif$longitude), ]
  query_gbif <- query_gbif[!(is.na(query_gbif$longitude)), ]
  for (i in 1:NROW(temp)) {
    if (!is.na(temp$verbatimLongitude[i]))
      temp$longitude[i] <- temp$verbatimLongitude[i]
  }
  query_gbif <- rbind(query_gbif, temp)

  query_gbif$aggregator <- "GBIF"
  query_gbif <- query_gbif %>%
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

  return(query_gbif)
}
