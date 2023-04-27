#' @title Used in gators_download() - Download data from Integrated Digitized Biocollections
#'
#' @description
#' The `get_idigbio()` function queries iDigBio for your desired species.
#' Limited to 100,000 record downloads.
#'
#' @details
#' This function uses the `correct_class()` function.
#' This function requires the packages ridigbio, magrittr, and dplyr.
#'
#' @param synonyms.list A list of affiliated names for your query.
#' @param limit Default = 100,000 (maximum). Set limit to the number of records requested for each element in synonyms.list.
#'
#' @examples
#' df <- get_idigbio(c("Galax urceolata", "Galax aphylla"), limit = 100)
#'
#' @return A data frame with desired columns from iDigBio.
#'
#' @importFrom dplyr bind_rows rename
#' @importFrom ridigbio idig_search_records
#' @importFrom magrittr "%>%"
#'
#' @export


get_idigbio <- function(synonyms.list, limit = 100000){
  if (length(synonyms.list) == 0 | any(is.na(synonyms.list))) {
    stop("Invalid argument: synonyms.list. The argument synonyms.list must be non-empty.")
  }

  colNames <- c("data.dwc:scientificName",
                "data.dwc:genus",
                "data.dwc:specificEpithet",
                "data.dwc:infraspecificEpithet",
                "uuid",
                "data.dwc:occurrenceID",
                "data.dwc:basisOfRecord",
                "data.dwc:eventDate",
                "data.dwc:year",
                "data.dwc:month",
                "data.dwc:day",
                "data.dwc:institutionCode",
                "data.dwc:recordedBy",
                "data.dwc:country",
                "data.dwc:county",
                "data.dwc:stateProvince",
                "data.dwc:locality",
                "data.dwc:occurrenceRemarks",
                "data.dwc:verbatimLocality",
                "data.dwc:decimalLatitude",
                "data.dwc:verbatimLatitude",
                "data.dwc:decimalLongitude",
                "data.dwc:verbatimLongitude",
                "geopoint",
                "data.dwc:coordinateUncertaintyInMeters",
                "data.dwc:informationWithheld",
                "data.dwc:habitat")
  query_idigbio <- data.frame(matrix(ncol = length(colNames), nrow = 0))
  colnames(query_idigbio) <- colNames

  for (i in 1:length(synonyms.list)) {
    query_idigbio <- rbind(query_idigbio, ridigbio::idig_search_records(rq = list("data" =  list("type" = "fulltext","value" = synonyms.list[i])), fields = colNames, limit = limit))
  }

  # if no results found
  if (NROW(query_idigbio) == 0) {
    message("No results found in iDigBio.")
    return(query_idigbio)
  }

  temp <- data.frame(matrix(NA, ncol = 0, nrow = 0))
  tempColNames <- colnames(temp)
  for (i in 1:length(colNames)) {
    if (colNames[i] == "geopoint") {
      if (!("geopoint.lon" %in% colnames(query_idigbio))) {
        temp <- data.frame(matrix(NA, ncol = NCOL(temp) + 1, nrow = 0))
        colnames(temp) <- c(tempColNames, "geopoint.lon")
        tempColNames <- colnames(temp)
      }
      if (!("geopoint.lat" %in% colnames(query_idigbio))) {
        temp <- data.frame(matrix(NA, ncol = NCOL(temp) + 1, nrow = 0))
        colnames(temp) <- c(tempColNames, "geopoint.lat")
        tempColNames <- colnames(temp)
      }
    }
    else if (!(colNames[i] %in% colnames(query_idigbio))) {
      temp <- data.frame(matrix(NA, ncol = NCOL(temp) + 1, nrow = 0))
      colnames(temp) <- c(tempColNames, colNames[i])
      tempColNames <- colnames(temp)
    }
  }
  query_idigbio <- dplyr::bind_rows(temp, query_idigbio)

  query_idigbio <- query_idigbio %>%
  dplyr::rename(scientificName = "data.dwc:scientificName",
                genus = "data.dwc:genus",
                specificEpithet = "data.dwc:specificEpithet",
                infraspecificEpithet = "data.dwc:infraspecificEpithet",
                ID = "uuid",
                occurrenceID = "data.dwc:occurrenceID",
                basisOfRecord =  "data.dwc:basisOfRecord",
                eventDate = "data.dwc:eventDate",
                year = "data.dwc:year",
                month = "data.dwc:month",
                day = "data.dwc:day",
                institutionCode = "data.dwc:institutionCode",
                recordedBy = "data.dwc:recordedBy",
                country = "data.dwc:country",
                county = "data.dwc:county",
                stateProvince = "data.dwc:stateProvince",
                locality = "data.dwc:locality",
                occurrenceRemarks = "data.dwc:occurrenceRemarks",
                verbatimLocality = "data.dwc:verbatimLocality",
                latitude = "data.dwc:decimalLatitude",
                verbatimLatitude = "data.dwc:verbatimLatitude",
                longitude = "data.dwc:decimalLongitude",
                verbatimLongitude = "data.dwc:verbatimLongitude",
                coordinateUncertaintyInMeters = "data.dwc:coordinateUncertaintyInMeters",
                informationWithheld = "data.dwc:informationWithheld",
                habitat = "data.dwc:habitat") %>% suppressWarnings(correct_class())

  # Add occurrenceRemarks, verbatimLocality to locality column
  query_idigbio$locality <- paste0("Locality: ", query_idigbio$locality)
  query_idigbio$locality <- paste(query_idigbio$locality, query_idigbio$occurrenceRemarks, sep = " Occurrence Remarks: ")
  query_idigbio$locality <- paste(query_idigbio$locality, query_idigbio$verbatimLocality, sep = " Verbatim Locality: ")

  # if decimal lat/lon columns are empty, replace with verbatim lat/lon columns or geopoint lat/lon
  temp <- query_idigbio[is.na(query_idigbio$latitude), ]
  if (NROW(temp) > 0) {
    query_idigbio <- query_idigbio[!(is.na(query_idigbio$latitude)), ]
    for (i in 1:NROW(temp)) {
      if (!is.na(temp$geopoint.lat[i]))
          temp$latitude[i] <- temp$geopoint.lat[i]
      else if (!is.na(temp$verbatimLatitude[i]))
        temp$latitude[i] <- temp$verbatimLatitude[i]
    }
    query_idigbio <- rbind(query_idigbio, temp)
  }

  temp <- query_idigbio[is.na(query_idigbio$longitude), ]
  if (NROW(temp) > 0) {
    query_idigbio <- query_idigbio[!(is.na(query_idigbio$longitude)), ]
    for (i in 1:NROW(temp)) {
      if (!is.na(temp$geopoint.lon[i]))
        temp$longitude[i] <- temp$geopoint.lon[i]
      else if (!is.na(temp$verbatimLongitude[i]))
          temp$longitude[i] <- temp$verbatimLongitude[i]
    }
    query_idigbio <- rbind(query_idigbio, temp)
  }

  #Remove these columns: occurrenceRemarks, verbatimLocality, verbatimLatitude, verbatimLongitude, geopoint.lat, geopoint.lon
  query_idigbio <- query_idigbio[, -which(names(query_idigbio) %in% c("occurrenceRemarks",
                                                                      "verbatimLatitude",
                                                                      "verbatimLongitude",
                                                                      "geopoint",
                                                                      "geopoint.lat",
                                                                      "geopoint.lon",
                                                                      "verbatimLocality"))]

  query_idigbio$aggregator <- "iDigBio"
  query_idigbio <- suppressWarnings(correct_class(query_idigbio))

  return(query_idigbio)
}
