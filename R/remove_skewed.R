#' @title Used in basic_locality_clean() - Removed skewed locality
#'
#' @description
#' The `remove_skewed()` function identifies and removes records where locality has been skewed.
#'
#' @details
#' This function requires no additional packages.
#'
#' @param df A data frame downloaded with `gators_download()`.
#' @inheritParams correct_class
#'
#' @examples
#' cleaned_data <- remove_skewed(data)
#'
#' @return A data frame with records remove only records for which locality was skewed.
#'
#' @export

remove_skewed <- function(df, info.withheld = "informationWithheld"){
  df <- df[grepl("Coordinate uncertainty increased", df[[info.withheld]]) == FALSE, ]
  return(df)
}

