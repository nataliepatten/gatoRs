#' @title needed_records
#'
#' @description
#' This function identifies records with flags
#'
#' @param occurrence_records is a data frame downloaded with spocc_combine
#'
#' @return a data frame with rows that locality was flagged
#'
#' @export

needed_records <- function(occurrence_records){

  information_needed <- occurrence_records %>%
                        dplyr::filter(informationWithheld != "NA") %>%
                        dplyr::filter(prov == "idigbio")
  uuid_list <- information_needed$ID
  idigbio_info <- dplyr::bind_rows(lapply(uuid_list, function(x) ridigbio::idig_search_records(rq = list(uuid = x), fields=c("uuid", "data.dwc:catalogNumber", "scientificname", "collectionname", "data.dwc:datasetName",  "data.dwc:institutionCode"))))
  return(idigbio_info)
}
