#' @title Download specimen data from both iDigBio and GBIF
#'
#' @description
#' The `gators_download()` function downloads data from GBIF and iDigBio for your desired species. Data returned includes:
#' scientificName, genus, specificEpithet, infraspecificEpithet, basisOfRecord, eventDate, institutionCode, collectionCode,
#' collectionID, country, county, stateProvince, locality, latitude, longitude, identificationID,
#' coordinateUncertaintyInMeters, informationWithheld, and habitat.
#'
#' @details
#' This function uses the `getidigbio()`, `getgbif()`, `fix_columns()`, `fix_names()`, and `filter_fix_names()` functions.
#' This function requires packages magrittr, rgbif, dplyr, ridigbio, and stringr.
#'
#'
#' @param synonyms_list A list of synonyms for your desired species. For example, `synonyms_list = c("Asclepias curtissii","Asclepias aceratoides", "Asclepias arenicola", "Oxypteryx arenicola", "Oxypteryx curtissii")`.
#' This parameter is required.
#'
#' @param write_file A parameter to choose whether to produce a .csv file containing search results.
#' This parameter is not required and is assigned FALSE by default.
#'
#' @param newFileName The path and file name for the retrieved data. Note that this parameter should include the ".csv"
#' extension as well. For example, `newFileName = "base_folder/other_folder/my_file.csv"`. The file path can be entered
#' either as relative to the current working directory (example: "../my_file.csv") or as a full path. This parameter is
#' required if `write_file = TRUE`.
#'
#' @param gbif_match A parameter to select either search by fuzzy matching of scientific name or to search by species code.
#' For example, `gbif_match = "fuzzy"` will search by fuzzy match and `gbif_match = "code"` will search by code. This parameter
#' is not required and is assigned "fuzzy" by default.
#'
#' @param idigbio_filter A parameter to remove less relevant search results from iDigBio. Based on the search input, results may
#' include data points for a different species that mention the desired species in the locality information, for example.
#' Choosing `idigbio_filter = TRUE` will return the data frame with rows in which the name column fuzzy matches a name on the synonym list.
#' This parameter is not required and is assigned TRUE by default.
#'
#'
#' @examples
#' df <- gators_download(synonyms_list = c("Galax urceolata", "Galax aphylla"), write_file = TRUE, newFileName = "galax.csv", idigbio_filter = FALSE)
#' df <- gators_download(synonyms_list = c("Galax urceolata", "Galax aphylla"), gbif_match = "code")
#'
#' @return Returns a data frame and writes a csv file as specified in the input.
#' This csv file will contain search results for the desired species
#' from the GBIF and iDigBio databases. The columns are as follows:
#' * [scientificName](http://rs.tdwg.org/dwc/terms/scientificName)
#' * [genus](https://dwc.tdwg.org/list/#dwc_genus)
#' * [specificEpithet](https://dwc.tdwg.org/list/#dwc_specificEpithet)
#' * [infraspecificEpithet](http://rs.tdwg.org/dwc/terms/infraspecificEpithet)
#' * [basisOfRecord](http://rs.tdwg.org/dwc/terms/basisOfRecord)
#' * [eventDate](http://rs.tdwg.org/dwc/terms/eventDate)
#' * [institutionCode](http://rs.tdwg.org/dwc/terms/institutionCode)
#' * [collectionCode](http://rs.tdwg.org/dwc/terms/collectionCode)
#' * [collectionID](http://rs.tdwg.org/dwc/terms/collectionID)
#' * [identificationID](http://rs.tdwg.org/dwc/terms/identificationID)
#' * [informationWithheld](http://rs.tdwg.org/dwc/terms/informationWithheld)
#' * [country](http://rs.tdwg.org/dwc/terms/country)
#' * [county](http://rs.tdwg.org/dwc/terms/county)
#' * [stateProvince](http://rs.tdwg.org/dwc/terms/stateProvince)
#' * [locality](http://rs.tdwg.org/dwc/terms/locality)
#' * [latitude](http://rs.tdwg.org/dwc/terms/decimalLatitude)
#' * [longitude](http://rs.tdwg.org/dwc/terms/decimalLongitude)
#' * [coordinateUncertaintyInMeters](http://rs.tdwg.org/dwc/terms/coordinateUncertaintyInMeters)
#' * [habitat](http://rs.tdwg.org/dwc/terms/habitat)
#'
#' @export


gators_download <- function(synonyms_list, write_file = FALSE, newFileName = NA, gbif_match = "fuzzy", idigbio_filter = TRUE) {
  # Check for valid arguments
  if (gbif_match != "fuzzy" & gbif_match != "code") {
    stop("Invalid value for argument: gbif_match. Value for gbif_match must equal 'fuzzy' or 'code'.")
  }

  if (idigbio_filter != TRUE & idigbio_filter != FALSE) {
    stop("Invalid value for argument: idigbio_filter. Value for idigbio_filter must equal 'TRUE' or 'FALSE'.")
  }

  if (write_file != TRUE & write_file != FALSE) {
    stop("Invalid value for argument: write_file. Value for write_file must equal 'TRUE' or 'FALSE'.")
  }
  else if (write_file) {
    if (is.na(newFileName)) {
      stop("Invalid value for argument: newFileName. The location and name of the output file is not specified.")
    }

    if (grepl(".csv", newFileName) == FALSE) {
      stop("Invalid value for argument: newFileName. The output file name must end in '.csv'.")
    }
  }
  else if (! is.na(newFileName)) {
    message("Warning: No output file will be written; the newFileName argument will be ignored.\nTo write to an output file, set write_file = TRUE.")
  }

  # initial download, fix capitalization
  query_idigbio <- fix_names(getidigbio(synonyms_list))
  query_gbif <- fix_names(getgbif(synonyms_list, gbif_match))

  # fill out remaining taxon columns, and fix capitalization again
  query_gbif <- fix_names(fix_columns(query_gbif))
  query_idigbio <- fix_names(fix_columns(query_idigbio))

  if (idigbio_filter) {
    query_idigbio <- filter_fix_names(query_idigbio, synonyms_list)
  }
  else {
    message("Warning: iDigBio search will return all records where any column has a matching string to the provided scientific names.")
  }

  # all queries contain records
  if (NROW(query_gbif) > 0 & NROW(query_idigbio) > 0) {
    output <- bind_rows(query_gbif, query_idigbio)
  }
  # only iDigBio contains records
  else if (NROW(query_idigbio) > 0) {
    output <- query_idigbio
  }
  # only GBIF contains records
  else if (NROW(query_gbif) > 0) {
    output <- query_gbif
  }
  # no queries contain records
  else {
    stop("No records found.")
  }

  if (write_file) {
    write.csv(output, newFileName, row.names = FALSE)
  }
  return(output)
}
