#' @title Remove Duplicates - Remove records with identical event dates and coordinates
#'
#' @description
#' The `remove_duplicates()` function removes records with identical event dates and occurrence IDs.
#' Prior to utilizing this function, longitude and latitude columns should be rounded to match the
#' coordinate uncertainty using the `basic_locality_clean()` function.
#'
#' @details
#' This function requires the lubridate and dplyr packages.
#'
#' @param df Data frame of occurrence records returned from `gators_download()`.
#' @inheritParams correct_class
#' @param remove.NA.occ.id Default = FALSE. This will remove records with missing occurrence IDs when set to `TRUE`.
#' @param remove.NA.date Default = FALSE. This will remove records with missing event dates when set to `TRUE`.
#'
#' @examples
#' data <- remove_duplicates(data)
#'
#' @return Return data frame with duplicates removed.
#'
#' @importFrom dplyr distinct
#'
#' @export

remove_duplicates <- function(df, event.date = "eventDate",
                              latitude = "latitude", longitude = "longitude",
                              aggregator = "aggregator", id = "ID", occ.id = "occurrenceID",
                              year = "year", month = "month", day = "day",
                              remove.NA.occ.id = FALSE, remove.NA.date = FALSE){

  if (NROW(df) == 0) return(df)

  if (remove.NA.occ.id == TRUE) {
    df <- df[!is.na(df[[occ.id]]), ]
  }
  if (remove.NA.date == TRUE) {
    df <- df[!is.na(df[[event.date]]), ]
  }

  # Remove within aggregation duplicates based on ID (UUID or KEY)
  ## This is done within our download function as well
  ag <- unique(df[[aggregator]])
  tempdf <- c()
  for(i in 1:length(ag)){
    tempdf[[i]] <- df[df[aggregator] == ag[i], ]
    if( (length(unique(na.omit(tempdf[[i]][[id]])))) != nrow(na.omit(tempdf[[i]][id])) ) {
      tempdf[[i]] <- dplyr::distinct(tempdf[[i]], .data[[id]], .keep_all = TRUE)
    }
  }
  df <- do.call(rbind, tempdf)

  # Remove specimen duplicates

  # Populate event.date column based on year, month, day columns
  df[[event.date]] <- dplyr::case_when(is.na(df[[event.date]]) ~ paste(df[[year]], df[[month]], df[[day]], sep="-"),
                                      .default = as.character(df[[event.date]]))

  for(i in 1:nrow(df)){
   if(!is.na(df[[event.date]][i])){
     # df[[event.date]][i] <- parse event date
     #parsedate::parse_iso_8601(parsedate::format_iso_8601(df[[event.date]][i]))
   }
  }
  # Remove rows with identical occurrence ID and date
  df <- dplyr::distinct(df, .data[[occ.id]], .data[[event.date]], .keep_all = TRUE)

  return(df)
}
