#' @title gators_download
#'
#' @description
#' This function downloads data from GBIF and iDigBio for your desired species. Data returned includes:
#' scientificName, genus, specificEpithet, infraspecificEpithet, basisOfRecord, eventDate, institutionCode, collectionCode,
#' collectionID, country, county, stateProvince, locality, latitude, longitude, identificationID,
#' coordinateUncertaintyInMeters, informationWithheld, and habitat.
#'
#' @details
#' This function uses the getidigbio, getgbif, fix_columns, fix_names, and filter_fix_names functions.
#' This function requires packages base, magrittr, utils, rgbif, dplyr, ridigbio, and stringr.
#'
#'
#' @param synonyms_list is a list of synonyms for your desired species. For example, `synonyms_list = c("Asclepias curtissii","Asclepias aceratoides", "Asclepias arenicola", "Oxypteryx arenicola", "Oxypteryx curtissii")`.
#' This parameter is required.
#'
#' @param newFileName is the path and file name for the retrieved data. Note that this parameter should include the ".csv"
#' extension as well. For example, `newFileName = "base_folder/other_folder/my_file.csv"`. The file path can be entered
#' either as relative to the current working directory (example: "../my_file.csv") or as a full path. This parameter is
#' required.
#'
#' @param gbif_match is a parameter to select either search by fuzzy matching of scientific name or to search by species code.
#' For example, `gbif_match = "fuzzy"` will search by fuzzy match and `gbif_match = "code"` will search by code. This parameter
#' is not required and is assigned "fuzzy" by default.
#'
#' @param idigbio_filter is a parameter to remove less relevant search results from iDigBio. Based on the search input, results may
#' include data points for a different species that mention the desired species in the locality information, for example.
#' Choosing `idigbio_filter = TRUE` will return the data frame with rows in which the name column fuzzy matches a name on the synonym list.
#' This parameter is not required and is assigned TRUE by default.
#'
#'
#' @examples
#' gators_download(c("Asclepias curtissi","Asclepias aceratoides"), "base/newFile.csv", gbif_match = "code")
#' gators_download(c("Asclepias curtissi","Asclepias aceratoides"), "base/other/my_new_file.csv", idigbio_filter = FALSE)
#'
#' @return Writes a csv file as specified in the input. This csv file will contain search results for the desired species
#' from the GBIF and iDigBio databases.
#'
#' @export


gators_download <- function(synonyms_list, newFileName, gbif_match = "fuzzy", idigbio_filter = TRUE) {

  # initial download, fix capitalization
  query_idigbio <- fixnames(getidigbio(synonyms_list))
  query_gbif <- fixnames(getgbif(synonyms_list, gbif_match))
  query_bien <- fixnames(getbien(synonyms_list))

  # fill out remaining taxon columns, and fix capitalization again
  query_gbif <- fix_names(fix_columns(query_gbif))
  query_idigbio <- fix_names(fix_columns(query_idigbio))
  query_bien <- fixnames(fix_columns(query_bien))

  if (idigbio_filter == TRUE) {
    query_idigbio <- filter_fix_names(query_idigbio, synonyms_list)
  }
  else {
    query_idigbio  <- query_idigbio
    print("Warning: iDigBio search will return all records where any column has a matching string to the provided scientific names")
  }

  # all queries contain records
  if (NROW(query_gbif) > 0 & NROW(query_idigbio) > 0 & NROW(query_bien) > 0)
    write.csv(bind_rows(query_idigbio, query_gbif, query_bien), newFileName, row.names = FALSE)
  # GBIF and BIEN contain records
  else if (NROW(query_gbif) > 0 & query_bien > 0)
    write.csv(bind_rows(query_gbif, query_bien), newFileName, row.names = FALSE)
  # GBIF and iDigBio contain records
  else if (NROW(query_gbif) > 0 & query_idigbio > 0)
    write.csv(bind_rows(query_gbif, query_idigbio), newFileName, row.names = FALSE)
  # BIEN and iDigBio contain records
  else if (NROW(query_bien) > 0 & query_idigbio > 0)
    write.csv(bind_rows(query_bien, query_idigbio), newFileName, row.names = FALSE)
  #iDigBio contains records
  else if (NROW(query_idigbio) > 0)
    write.csv(query_idigbio, newFileName, row.names = FALSE)
  # BIEN contains records
  else if (NROW(query_bien) > 0)
    write.csv(query_bien, newFileName, row.names = FALSE)
  #GBIF contains records
  else if (NROW(query_gbif) > 0)
    write.csv(query_gbif, newFileName, row.names = FALSE)
  # no queries contain records
  else
    print("No records found.")
}
