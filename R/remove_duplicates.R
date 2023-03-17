#' @title Remove Duplicates - Remove records with identical event dates and coordinates
#'
#' @description
#' The `remove_duplicates()` function removes records with identical event dates and coordinate values.
#' Prior to utilizing this function, longitude and latitude columns should be rounded to match the coordinate uncertainty using the `basic_locality_clean()` function.
#'
#' @param df Data frame of occurrence records returned from `gators_download()`.
#'
#' @examples
#' data %>% remove_duplicates()
#'
#' @return Return data frame with duplicates removed
#'
#' @importFrom lubridate ymd year month day
#' @importFrom dplyr distinct
#'
#' @export
#'
#'

remove_duplicates <- function(df){
  # Parse date with Lubridate
  suppressWarnings(df$eventDate <-  lubridate::ymd(df$eventDate))
  df$year <- lubridate::year(df$eventDate)
  df$month <- lubridate::month(df$eventDate)
  df$day <- lubridate::day(df$eventDate)
  # Remove rows with identical latitude, longitude, year, month, and day
  df <- distinct(df, latitude, longitude, year, month, day, .keep_all = TRUE)
  # Removes extra columns
  df <- df[ , -which(names(df) %in% c("year", "month", "day"))]
  return(df)

}
