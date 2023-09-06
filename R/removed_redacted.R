#' @title Remove Redacted Information - Remove protected or private records prior to publication
#'
#' @description
#' The `remove_redacted()` function identifies and removes records where 'aggregator' is not equal to iDigBio or GBIF.
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
#' Information about the columns in the returned data frame can be found in the documentation for `gators_download()`.
#'
#' @export

remove_redacted <- function(df, aggregator = "aggregator"){
  if (NROW(df) == 0) return(df)

  ready <- df[which(df[[aggregator]] %in% c("iDigBio", "GBIF")), ]
  return(ready)
}
