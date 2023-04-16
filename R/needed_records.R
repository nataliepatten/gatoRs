#' @title Identify Missing Information - Find records with redacted or missing data
#'
#' @description
#' The `needed_records()` function identifies records with flags. This indicates that information
#' is withheld from these records due to endangered species status, for example. Accessing this information may
#' require a permit. Or, these records can be removed from the data set.
#'
#' @details
#' This function requires no additional packages.
#'
#' @param df A data frame downloaded with `gators_download()`.
#' @inheritParams correct_class
#'
#' @examples
#' need_info <- needed_records(data)
#'
#' @return A data frame with only records for which  locality was flagged.
#'
#' @export

needed_records <- function(df, info.withheld = "informationWithheld"){
  if (NROW(df) == 0) return(df)

  information_needed <- df[!is.na(df[[info.withheld]]), ]
  return(information_needed)
}
