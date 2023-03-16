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
#' @param newFileName The path and file name for the retrieved data. Note that this parameter should include the ".csv"
#' extension as well. For example, `newFileName = "base_folder/other_folder/my_file.csv"`. The file path can be entered
#' either as relative to the current working directory (example: "../my_file.csv") or as a full path. This parameter is
#' required.
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
#' gators_download(c("Asclepias curtissii", "Asclepias aceratoides", "Asclepias arenicola", "Oxypteryx arenicola", "Oxypteryx curtissii"), "my_new_file.csv", idigbio_filter = FALSE)
#' gators_download(c("Asclepias curtissii", "Asclepias aceratoides", "Asclepias arenicola", "Oxypteryx arenicola", "Oxypteryx curtissii"), "newFile.csv", gbif_match = "code")
#'
#' @return Writes a csv file as specified in the input. This csv file will contain search results for the desired species
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


gators_download <- function(synonyms_list, newFileName, gbif_match = "fuzzy", idigbio_filter = TRUE) {
  # Check for valid arguments
  if (gbif_match != "fuzzy" & gbif_match != "code") {
    stop(print("Invalid value for argument: gbif_match. Value for gbif_match must equal 'fuzzy' or 'code'."))
  }

  if (idigbio_filter != TRUE & idigbio_filter != FALSE) {
    stop(print("Invalid value for argument: idigbio_filter. Value for idigbio_filter must equal 'TRUE' or 'FALSE'."))
  }

  if (is.na(newFileName) == TRUE) {
    stop(print("Invalid value for argument: newFileName. The location and name of the output file is not specified."))
  }

  if (grepl(".csv", newFileName) == FALSE) {
    stop(print("Invalid value for argument: newFileName. The output file name must end in .csv"))
  }

  # initial download, fix capitalization
  query_idigbio <- fix_names(getidigbio(synonyms_list))
  query_gbif <- fix_names(getgbif(synonyms_list, gbif_match))
  #query_bien <- fix_names(getbien(synonyms_list))

  # fill out remaining taxon columns, and fix capitalization again
  query_gbif <- fix_names(fix_columns(query_gbif))
  query_idigbio <- fix_names(fix_columns(query_idigbio))
  #query_bien <- fix_names(fix_columns(query_bien))

  if (idigbio_filter == TRUE) {
    query_idigbio <- filter_fix_names(query_idigbio, synonyms_list)
  }
  else {
    query_idigbio  <- query_idigbio
    print("Warning: iDigBio search will return all records where any column has a matching string to the provided scientific names.")
  }

  # all queries contain records
  #if (NROW(query_gbif) > 0 & NROW(query_idigbio) > 0 & NROW(query_bien) > 0)
  #  write.csv(bind_rows(query_idigbio, query_gbif, query_bien), newFileName, row.names = FALSE)
  # GBIF and BIEN contain records
  #else if (NROW(query_gbif) > 0 & query_bien > 0)
  #  write.csv(bind_rows(query_gbif, query_bien), newFileName, row.names = FALSE)
  # GBIF and iDigBio contain records
  if (NROW(query_gbif) > 0 & NROW(query_idigbio) > 0){
    output <- bind_rows(query_gbif, query_idigbio)
    write.csv(output, newFileName, row.names = FALSE)
  # BIEN and iDigBio contain records
  #else if (NROW(query_bien) > 0 & query_idigbio > 0)
  #  write.csv(bind_rows(query_bien, query_idigbio), newFileName, row.names = FALSE)
  #iDigBio contains records
  }else if (NROW(query_idigbio) > 0){
    output <- query_idigbio
    write.csv(query_idigbio, newFileName, row.names = FALSE)
  # BIEN contains records
  #else if (NROW(query_bien) > 0)
  #  write.csv(query_bien, newFileName, row.names = FALSE)
  #GBIF contains records
  }else if (NROW(query_gbif) > 0){
    output <- query_gbif
    write.csv(query_gbif, newFileName, row.names = FALSE)
  # no queries contain records
  }else{
    print("No records found.")
  }
  return(output)
}
