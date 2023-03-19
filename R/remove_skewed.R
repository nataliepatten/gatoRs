#' @title Used in basic_locality_clean() - Removed skewed locality
#'
#' @description
#' The `remove_skewed()` function identifies and removes records where locality has been skewed.
#'
#' @param df A data frame downloaded with `gators_download()`.
#'
#'
#' @return A data frame with records remove only records for which locality was skewed.
#'
#' @export

remove_skewed <- function(df){
  df <- df[grepl("Coordinate uncertainty increased", df$informationWithheld) == FALSE, ]
  return(df)
}

