#' @title Download - Download specimen data from both iDigBio and GBIF
#'
#' @description
#' The `gators_download()` function downloads data from GBIF and iDigBio for your desired species. Data returned includes:
#' scientificName, genus, specificEpithet, infraspecificEpithet, basisOfRecord, eventDate, institutionCode, collectionCode,
#' collectionID, country, county, stateProvince, locality, latitude, longitude, identificationID,
#' coordinateUncertaintyInMeters, informationWithheld, and habitat.
#'
#' @details
#' This function uses the `get_idigbio()`, `get_gbif()`, `fix_columns()`, `fix_names()`, and `filter_fix_names()` functions.
#' This function requires packages magrittr, rgbif, dplyr, ridigbio, and stringr.
#'
#'
#' @param synonyms.list A list of synonyms for your desired species. For example, `synonyms.list = c("Asclepias curtissii","Asclepias aceratoides", "Asclepias arenicola", "Oxypteryx arenicola", "Oxypteryx curtissii")`.
#' This parameter is required.
#'
#' @param write.file A parameter to choose whether to produce a .csv file containing search results.
#' This parameter is not required and is assigned FALSE by default.
#'
#' @param filename The path and file name for the retrieved data. Note that this parameter should include the ".csv"
#' extension as well. For example, `filename = "base_folder/other_folder/my_file.csv"`. The file path can be entered
#' either as relative to the current working directory (example: "../my_file.csv") or as a full path. This parameter is
#' required if `write.file = TRUE`.
#'
#' @param gbif.match A parameter to select either search by fuzzy matching of scientific name or to search by species code.
#' For example, `gbif.match = "fuzzy"` will search by fuzzy match and `gbif.match = "code"` will search by code. This parameter
#' is not required and is assigned "fuzzy" by default.
#'
#' @param idigbio.filter A parameter to remove less relevant search results from iDigBio. Based on the search input, results may
#' include data points for a different species that mention the desired species in the locality information, for example.
#' Choosing `idigbio.filter = TRUE` will return the data frame with rows in which the name column fuzzy matches a name on the synonym list.
#' This parameter is not required and is assigned TRUE by default.
#'
#' @inheritParams correct_class
#'
#' @examples
#' df <- gators_download(synonyms.list = c("Galax urceolata", "Galax aphylla"), write.file = TRUE, filename = "galax.csv")
#' df <- gators_download(synonyms.list = "Galax urceolata", gbif.match = "code", idigbio.filter = FALSE)
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


gators_download <- function(synonyms.list, write.file = FALSE, filename = NA,
                            gbif.match = "fuzzy", idigbio.filter = TRUE,
                            scientific.name = "scientificName", genus = "genus",
                            species = "specificEpithet", infraspecific.epithet = "infraspecificEpithet") {

  # Check for valid arguments
  if (length(synonyms.list) == 0 | any(is.na(synonyms.list))) {
    stop("Invalid argument: synonyms.list. The argument synonyms.list must be non-empty.")
  }

  if (gbif.match != "fuzzy" & gbif.match != "code") {
    stop("Invalid value for argument: gbif.match. Value for gbif.match must equal 'fuzzy' or 'code'.")
  }

  if (idigbio.filter != TRUE & idigbio.filter != FALSE) {
    stop("Invalid value for argument: idigbio.filter. Value for idigbio.filter must equal 'TRUE' or 'FALSE'.")
  }

  if (write.file != TRUE & write.file != FALSE) {
    stop("Invalid value for argument: write.file. Value for write.file must equal 'TRUE' or 'FALSE'.")
  }
  else if (write.file) {
    if (is.na(filename)) {
      stop("Invalid value for argument: filename. The location and name of the output file is not specified.")
    }

    if (grepl(".csv", filename) == FALSE) {
      stop("Invalid value for argument: filename. The output file name must end in '.csv'.")
    }
  }
  else if (! is.na(filename)) {
    message("Warning: No output file will be written; the filename argument will be ignored.\nTo write to an output file, set write.file = TRUE.")
  }

  # initial download, fix capitalization
  query_idigbio <- fix_names(get_idigbio(synonyms.list), scientific.name = scientific.name)
  query_gbif <- fix_names(get_gbif(synonyms.list, gbif.match), scientific.name = scientific.name)

  # fill out remaining taxon columns, and fix capitalization again
  query_gbif <- fix_names(fix_columns(query_gbif, scientific.name = scientific.name,
                                      genus = genus, species = species, infraspecific.epithet = infraspecific.epithet),
                          scientific.name = scientific.name)
  query_idigbio <- fix_names(fix_columns(query_idigbio, scientific.name = scientific.name,
                                         genus = genus, species = species, infraspecific.epithet = infraspecific.epithet),
                             scientific.name = scientific.name)

  if (idigbio.filter) {
    query_idigbio <- filter_fix_names(query_idigbio, synonyms.list, scientific.name = scientific.name)
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

  if (write.file) {
    write.csv(output, filename, row.names = FALSE)
  }
  return(output)
}
