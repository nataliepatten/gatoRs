#' @title need_to_georeference
#'
#' @description
#' This function allows you to find the points that are missing coordinates
#' but have a locality.
#'
#' @details
#' This function requires packages dplyr, base, magrittr.
#' This function does not require user input.
#'
#' @param occurrence_records is a dataframe of occurrence records
#'
#' @return for_georeferencing_all is a dataframe of the points that need to be georeferenced
#'
#' @importFrom dplyr coalesce
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr bind_rows
#' @importFrom  dplyr left_join
#'


need_to_georeference <- function(occurrence_records){
  occurrence_records$Latitude <- dplyr::coalesce(occurrence_records$Latitude,
                                          occurrence_records$spocc.latitude)
  occurrence_records$Longitude <- dplyr::coalesce(occurrence_records$Longitude,
                                           occurrence_records$spocc.longitude)
  for_georeferencing <- occurrence_records %>%
                        dplyr::filter(locality != 'NA') %>%
                        dplyr::filter(is.na(Latitude)) %>%
                        dplyr::filter(is.na(Longitude)) %>%
                        dplyr::rename(uuid = ID)
  uuid_list <- for_georeferencing$uuid
  idigbio_info <- dplyr::bind_rows(lapply(uuid_list, function(x) idig_search_records(rq = list(uuid = x), fields=c("uuid", "data.dwc:catalogNumber", "scientificname", "collectionname", "data.dwc:datasetName",  "data.dwc:institutionCode"))))
  for_georeferencing_all <- dplyr::left_join(for_georeferencing, idigbio_info, by = "uuid")
  return(for_georeferencing_all)
}
