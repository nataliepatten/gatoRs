#' @title Remove Duplicates - Remove records with identical event dates and coordinates
#'
#' @description
#' The `remove_duplicates()` function removes records with identical event dates and coordinate values.
#' Prior to utilizing this function, longitude and latitude columns should be rounded to match the coordinate uncertainty using the `basic_locality_clean()` function.
#'
#' @param df Data frame of occurrence records returned from `gators_download()`.
#' @inheritParams correct_class
#'
#' @examples
#' data <- remove_duplicates(data)
#'
#' @return Return data frame with duplicates removed.
#'
#' @importFrom lubridate ymd year month day
#' @importFrom dplyr distinct
#'
#' @export
#'
#'

remove_duplicates <- function(df, event.date = "eventDate"){
  if (NROW(df) == 0) return(df)

  # Parse date with Lubridate
  suppressWarnings(df[[event.date]] <-  lubridate::ymd(df[[event.date]]))
  df$year <- lubridate::year(df[[event.date]])
  df$month <- lubridate::month(df[[event.date]])
  df$day <- lubridate::day(df[[event.date]])
  # Remove rows with identical latitude, longitude, year, month, and day
  df <- dplyr::distinct(df, latitude, longitude, year, month, day, .keep_all = TRUE)
  # Removes extra columns
  df <- df[ , -which(names(df) %in% c("year", "month", "day"))]
  return(df)

}
