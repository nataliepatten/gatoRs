#' @title need_to_georeference
#'
#' @description
#' This function allows you to find records that are missing coordinates but have locality information.
#'
#' @details
#' This function requires packages dplyr, base, magrittr.
#' This function does not require user input.
#'
#' @param occurrence_records is a dataframe of occurrence records
#'
#' @return for_georeferencing_all is a dataframe of the points that need to be georeferenced
#'
#' @export


need_to_georeference <- function(occurrence_records){
  for_georeferencing <- occurrence_records %>%
    dplyr::filter(locality != 'NA') %>%
    dplyr::filter(is.na(latitude)) %>%
    dplyr::filter(is.na(longitude))
  # uuid_list <- for_georeferencing$uuid
  # idigbio_info <- dplyr::bind_rows(lapply(uuid_list, function(x) idig_search_records(rq = list(uuid = x), fields=c("uuid", "data.dwc:catalogNumber", "scientificname", "collectionname", "data.dwc:datasetName",  "data.dwc:institutionCode"))))
  # for_georeferencing_all <- dplyr::left_join(for_georeferencing, idigbio_info, by = "uuid")
  return(for_georeferencing)
}
