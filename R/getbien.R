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
#' @param synonyms_list is a list of affiliated names for your query.
#'
#' @return a data frame with desired columns from BIEN.
#'
#' @export


getbien <- function(synonyms_list){
  query_bien <- BIEN::BIEN_occurrence_species(synonyms_list[1])
  
  for (i in 2:length(synonyms_list)) {
     query_bien <- rbind(query_bien, BIEN::BIEN_occurrence_species(synonyms_list[i]))
  }
  
  query_bien <- query_bien %>%
    dplyr::rename(scientificName = "scrubbed_species_binomial",
                  eventDate = "date_collected")
      
  return(query_bien)
}