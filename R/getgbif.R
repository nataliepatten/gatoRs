#' @title Download data from the Global Biodiversity Information Facility
#'
#' @description
#' The `getgbif()` function queries the Global Biodiversity Information Facility (GBIF) for your desired species.
#' Limited to 100,000 record downloads.
#'
#' @details
#' This function uses the `correct_class()` function.
#' This function requires the packages rgbif, magrittr, and dplyr.
#'
#' @param synonyms_list A list of affiliated names for your query.
#' @param gbif_match Either "fuzzy" for fuzzy matching of name or "code" to search by species code.
#'
#' @return Returns a data frame with desired columns from GBIF.
#'
#' @importFrom dplyr bind_rows rename select
#' @importFrom rgbif occ_data name_backbone
#' @importFrom magrittr "%>%"
#'
#' @export


getgbif <- function(synonyms_list, gbif_match = "fuzzy"){
  if (gbif_match != "fuzzy" & gbif_match != "code") {
    stop("Invalid argument: gbif_match.")
  }

  colNamesFuzzy <- c("data.scientificName",
                     "data.genus",
                     "data.specificEpithet",
                     "data.infraspecificEpithet",
                     "data.basisOfRecord",
                     "data.eventDate",
                     "data.institutionCode",
                     "data.collectionCode",
                     "data.collectionID",
                     "data.country",
                     "data.county",
                     "data.stateProvince",
                     "data.locality",
                     "data.occurrenceRemarks",
                     "data.verbatimLocality",
                     "data.decimalLatitude",
                     "data.verbatimLatitude",
                     "data.decimalLongitude",
                     "data.verbatimLongitude",
                     "data.identificationID",
                     "data.coordinateUncertaintyInMeters",
                     "data.informationWithheld",
                     "data.habitat",
                     "data.geodeticDatum")
  colNamesCode <- c("scientificName",
                    "genus",
                    "specificEpithet",
                    "infraspecificEpithet",
                    "basisOfRecord",
                    "eventDate",
                    "institutionCode",
                    "collectionCode",
                    "collectionID",
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
                    "identificationID",
                    "coordinateUncertaintyInMeters",
                    "informationWithheld",
                    "habitat",
                    "geodeticDatum")

  if (gbif_match == "code") {
    key <- rgbif::name_backbone(name = synonyms_list[1])$speciesKey
    query_gbif <- rgbif::occ_data(taxonKey = key, limit = 100000)
  }
  else if (gbif_match == "fuzzy") {
    query_gbif <- data.frame(matrix(ncol = length(colNamesFuzzy), nrow = 0))
    colnames(query_gbif) <- colNamesFuzzy
    for (i in 1:length(synonyms_list)) {
      temp <- rgbif::occ_data(scientificName = synonyms_list[i], limit = 100000)
      temp <- data.frame(temp[2])
      query_gbif <- rbind(query_gbif, temp)
    }
  }

  temp <- data.frame(matrix(NA, ncol = 0, nrow = 0))
  tempColNames <- colnames(temp)
  if (gbif_match == "fuzzy") {
    colNames <- colNamesFuzzy
  }
  else if (gbif_match == "code") {
    colNames <- colNamesCode
  }
  for (i in 1:length(colNames)) {
    if (!(colNames[i] %in% colnames(query_gbif))) {
      temp <- data.frame(matrix(NA, ncol = NCOL(temp) + 1, nrow = 0))
      colnames(temp) <- c(tempColNames, colNames[i])
      tempColNames <- colnames(temp)
     }
  }

  for (i in 1:NCOL(temp)) {
    if (grepl("Latitude", colNames[i], ignore.case = TRUE) || grepl("Longitude", colNames[i], ignore.case = TRUE)
        || grepl("meters", colNames[i], ignore.case = TRUE)) {
      temp[,i] <- as.numeric(temp[,i])
    }
    else {
      temp[,i] <- as.character(temp[,i])
    }
  }

  if (gbif_match == "fuzzy") {
    query_gbif <- dplyr::bind_rows(temp, query_gbif)
    query_gbif <- query_gbif %>%
                dplyr::rename(scientificName = "data.scientificName",
                              genus = "data.genus",
                              specificEpithet = "data.specificEpithet",
                              infraspecificEpithet = "data.infraspecificEpithet",
                              basisOfRecord =  "data.basisOfRecord",
                              eventDate = "data.eventDate",
                              institutionCode = "data.institutionCode",
                              collectionCode = "data.collectionCode",
                              collectionID = "data.collectionID",
                              country = "data.country",
                              county = "data.county",
                              stateProvince = "data.stateProvince",
                              locality = "data.locality",
                              occurrenceRemarks = "data.occurrenceRemarks",
                              verbatimLocality = "data.verbatimLocality",
                              latitude = "data.decimalLatitude",
                              verbatimLatitude = "data.verbatimLatitude",
                              longitude = "data.decimalLongitude",
                              verbatimLongitude = "data.verbatimLongitude",
                              identificationID = "data.identificationID",
                              coordinateUncertaintyInMeters = "data.coordinateUncertaintyInMeters",
                              informationWithheld = "data.informationWithheld",
                              habitat = "data.habitat",
                              geodeticDatum = "data.geodeticDatum")
  }
  else if (gbif_match == "code") {
    query_gbif <- query_gbif$data
    query_gbif <- dplyr::bind_rows(temp, query_gbif)
    query_gbif <- dplyr::rename(query_gbif, latitude = "decimalLatitude", longitude = "decimalLongitude")
  }

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

  query_gbif <- query_gbif %>%
                dplyr::select("scientificName",
                  "genus",
                  "specificEpithet",
                  "infraspecificEpithet",
                  "basisOfRecord",
                  "eventDate",
                  "institutionCode",
                  "collectionCode",
                  "collectionID",
                  "country",
                  "county",
                  "stateProvince",
                  "locality",
                  "latitude",
                  "longitude",
                  "identificationID",
                  "coordinateUncertaintyInMeters",
                  "informationWithheld",
                  "habitat") %>%
                  correct_class()

  return(query_gbif)
}
