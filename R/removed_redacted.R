#' @title Remove Missing Information - Remove records with redacted or missing data
#'
#' @description
#' The `remove_redacted()` function identifies and removes records with flags. This indicates that information
#' is withheld from these records due to endangered species status, for example. Accessing this information may
#' require a permit.
#'
#' @details
#' This function requires no additional packages.
#'
#' @param df A data frame downloaded with `gators_download()`.
#' @inheritParams correct_class
#'
#' @examples
#' cleaned_data <- remove_redacted(data)
#'
#' @return A data frame with redacted records removed.
#'
#' @export

remove_redacted <- function(df, info.withheld = "informationWithheld"){
  if (NROW(df) == 0) return(df)

  cleaned <- df[is.na(df[[info.withheld]]), ]
  return(cleaned)
}
