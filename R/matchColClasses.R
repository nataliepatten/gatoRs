#' matchColClasses
#'
#' This function allows you to match the column classes of two data frames.
#' This function requires package base. This function does not require user input.
#' Source: https://stackoverflow.com/questions/49215193/r-error-cant-join-on-because-of-incompatible-types
#' @param df1 is a data frame
#' @param df2 is a data frame
#' @return df2 with corrected column classes
matchColClasses <- function(df1, df2) {
  sharedColNames <- names(df1)[names(df1) %in% names(df2)]
  sharedColTypes <- sapply(df1[,sharedColNames], class)
  for (n in sharedColNames) {
    class(df2[, n]) <- sharedColTypes[n]
  }
  return(df2)
}
