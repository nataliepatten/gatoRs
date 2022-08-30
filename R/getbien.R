#' @title getbien
#'
#' @description
#' This function queries BIEN for your desired species.
#'
#' @details
#' This function is used in the gators_download function.
#' This function uses the correct_class function.
#' This function requires the packages BIEN, magrittr, base, dplyr.
#'
#' @param synonyms_list A list of affiliated names for your query.
#'
#' @return a data frame with desired columns from BIEN.
#'
#' @importFrom BIEN BIEN_occurrence_species
#'
#' @export


getbien <- function(synonyms_list){
  query_bien <- BIEN::BIEN_occurrence_species(synonyms_list[1])

  if (length(synonyms_list > 1)) {
    for (i in 2:length(synonyms_list)) {
       query_bien <- rbind(query_bien, BIEN::BIEN_occurrence_species(synonyms_list[i]))
    }
  }

  query_bien <- query_bien %>%
    dplyr::rename(scientificName = "scrubbed_species_binomial",
                  eventDate = "date_collected")

  query_bien$genus <- NA
  query_bien$specificEpithet <- NA
  query_bien$infraspecificEpithet <- NA
  query_bien$basisOfRecord <- NA
  query_bien$eventDate <- NA
  query_bien$institutionCode <- NA
  query_bien$collectionCode <- NA
  query_bien$collectionID <- NA
  query_bien$country <- NA
  query_bien$county <- NA
  query_bien$stateProvince <- NA
  query_bien$locality <- NA
  query_bien$identificationID <- NA
  query_bien$coordinateUncertaintyInMeters <- NA
  query_bien$informationWithheld <- NA
  query_bien$habitat <- NA

  return(query_bien)
}
