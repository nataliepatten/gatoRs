#' @title Find records with redacted or missing data
#'
#' @description
#' The `needed_records()` function identifies records with flags. This indicates that information
#' is withheld from these records due to endangered species status, for example. Accessing this information may
#' require a permit. Or, these records can be removed from the data set.
#'
#' @details
#' This function requires packages dplyr, and magrittr.
#'
#' @param occurrence_records A data frame downloaded with `gators_download()`.
#'
#' @return a data frame with rows that locality was flagged
#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#'
#' @export

needed_records <- function(occurrence_records){

  information_needed <- occurrence_records %>%
                        dplyr::filter(informationWithheld != "NA") # %>%
                        # dplyr::filter(prov == "idigbio")
  # uuid_list <- information_needed$ID
  # idigbio_info <- dplyr::bind_rows(lapply(uuid_list, function(x) ridigbio::idig_search_records(rq = list(uuid = x), fields=c("uuid", "data.dwc:catalogNumber", "scientificname", "collectionname", "data.dwc:datasetName",  "data.dwc:institutionCode"))))
  return(information_needed)
}
