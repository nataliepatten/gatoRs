#' @title Download data from Integrated Digitized Biocollections
#'
#' @description
#' The `getidigbio()` function queries iDigBio for your desired species.
#' Limited to 100,000 record downloads.
#'
#' @details
#' This function uses the `correct_class()` function.
#' This function requires the packages ridigbio, magrittr, and dplyr.
#'
#' @param synonyms_list A list of affiliated names for your query.
#'
#' @examples
#' df <- getidigbio(c("Galax urceolata", "Galax aphylla"))
#'
#' @return A data frame with desired columns from iDigBio.
#'
#' @importFrom dplyr bind_rows rename
#' @importFrom ridigbio idig_search_records
#' @importFrom magrittr "%>%"
#'
#' @export


getidigbio <- function(synonyms_list){
  colNames <- c("data.dwc:scientificName",
                "data.dwc:genus",
                "data.dwc:specificEpithet",
                "data.dwc:infraspecificEpithet",
                "data.dwc:basisOfRecord",
                "data.dwc:eventDate",
                "institutioncode",
                "data.dwc:collectionCode",
                "data.dwc:collectionID",
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
                "uuid",
                "data.dwc:coordinateUncertaintyInMeters",
                "data.dwc:informationWithheld",
                "data.dwc:habitat")
  query_idigbio <- data.frame(matrix(ncol = length(colNames), nrow = 0))
  colnames(query_idigbio) <- colNames

  for (i in 1:length(synonyms_list)) {
    query_idigbio <- rbind(query_idigbio, ridigbio::idig_search_records(rq = list("data" =  list("type" = "fulltext","value" = synonyms_list[i])), fields = colNames, limit = 100000))
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
                basisOfRecord =  "data.dwc:basisOfRecord",
                eventDate = "data.dwc:eventDate",
                institutionCode = "institutioncode",
                collectionCode = "data.dwc:collectionCode",
                collectionID = "data.dwc:collectionID",
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
                identificationID = "uuid",
                coordinateUncertaintyInMeters = "data.dwc:coordinateUncertaintyInMeters",
                informationWithheld = "data.dwc:informationWithheld",
                habitat = "data.dwc:habitat")

  # Add occurrenceRemarks, verbatimLocality to locality column
  query_idigbio$locality <- paste0("Locality: ", query_idigbio$locality)
  query_idigbio$locality <- paste(query_idigbio$locality, query_idigbio$occurrenceRemarks, sep = " Occurrence Remarks: ")
  query_idigbio$locality <- paste(query_idigbio$locality, query_idigbio$verbatimLocality, sep = " Verbatim Locality: ")

  # if decimal lat/lon columns are empty, replace with verbatim lat/lon columns or geopoint lat/lon
  temp <- query_idigbio[is.na(query_idigbio$latitude), ]
  query_idigbio <- query_idigbio[!(is.na(query_idigbio$latitude)), ]
  for (i in 1:NROW(temp)) {
    if (!is.na(temp$geopoint.lat[i]))
        temp$latitude[i] <- temp$geopoint.lat[i]
    else if (!is.na(temp$verbatimLatitude[i]))
      temp$latitude[i] <- temp$verbatimLatitude[i]
  }
  query_idigbio <- rbind(query_idigbio, temp)

  temp <- query_idigbio[is.na(query_idigbio$longitude), ]
  query_idigbio <- query_idigbio[!(is.na(query_idigbio$longitude)), ]
  for (i in 1:NROW(temp)) {
    if (!is.na(temp$geopoint.lon[i]))
      temp$longitude[i] <- temp$geopoint.lon[i]
    else if (!is.na(temp$verbatimLongitude[i]))
        temp$longitude[i] <- temp$verbatimLongitude[i]
  }
  query_idigbio <- rbind(query_idigbio, temp)

  #Remove these columns: occurrenceRemarks, verbatimLocality, verbatimLatitude, verbatimLongitude, geopoint.lat, geopoint.lon
  query_idigbio <- query_idigbio[c(-14, -15, -17, -19, -20, -21)] %>% correct_class()
 return(query_idigbio)
}
