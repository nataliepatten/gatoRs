#' @title Find occurrence records which lack coordinate information
#'
#' @description
#' The `need_to_georeference()` function allows you to find records that are missing coordinates
#' but contain locality information. These records can then be manually georeferenced.
#'
#' @details
#' This function requires packages dplyr and magrittr.
#'
#' @param occurrence_records A data frame of occurrence records.
#'
#' @examples
#' need_coords <- need_to_georeference(data)
#'
#' @return Returns a data frame of the points that need to be georeferenced.
#' For more information about this data frame, see `gators_download()`.
#'
#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
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
