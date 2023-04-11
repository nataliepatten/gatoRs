#' @title Remove Duplicates - Remove records with identical event dates and coordinates
#'
#' @description
#' The `remove_duplicates()` function removes records with identical event dates and coordinate values.
#' Prior to utilizing this function, longitude and latitude columns should be rounded to match the
#' coordinate uncertainty using the `basic_locality_clean()` function.
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

remove_duplicates <- function(df, event.date = "eventDate",
                              latitude = "latitude", longitude = "longitude",
                              aggregator = "aggregartior", id = "ID", occurenceID = "occurenceID"){
  if (NROW(df) == 0) return(df)

  # Remove within aggregation duplicates based on ID (UUID or KEY)
  ## This is done within our download function as well
  ag <- unique(df[[aggregator]])
  tempdf <- c()
  for(i in 1:length(ag)){
    tempdf[[i]] <-  df[df[aggregator] == ag[i], ]
    if(((length(unique(na.omit(tempdf[[i]][[id]])))) == (nrow(na.omit(tempdf[[i]][id])))) == FALSE){
      tempdf[[i]] <- dplyr::distinct(tempdf[[i]], .data[[id]], .keep_all = TRUE)
    }
  }
  df <- do.call(rbind, tempdf)

  # Remove specimen duplicates
  # Format dates

  #for(i in 1:nrow(df)){
   # if(!is.na(df[[eventDate]])){

  #  dashcount <- stringr::str_count(random, "[-]")
  #  if(dashcount == 0){
    #  df$year[i] <- lubridate::year(as.Date(tempdate, "%Y"))
     # df$month[i] <- NA
     # df$day[i] <- NA
   # } else if(dashcount == 1){
   #   tempdate <- as.Date(tempdate, "%Y-%m")
   #   df$year[i] <- lubridate::year(tempdate)
  #    df$month[i] <- lubridate::month(tempdate)
   #   df$day[i] <- NA
   # }else if(dashcount == 2){
  #    tempdate <- as.Date(tempdate, "%Y-%m-%d")
  #    df$year[i] <- lubridate::year(tempdate)
   #   df$month[i] <- lubridate::month(tempdate)
  ##    df$day[i] <- lubridate::day(tempdate)
  #  }else{
# #     df$year <- NA
 #     df$month[i] <- NA
 #     df$day[i] <- NA}}
 # }

  # Parse date with Lubridate
  suppressWarnings(df[[event.date]] <-  lubridate::ymd(df[[event.date]]))
  df$year <- lubridate::year(df[[event.date]])
  df$month <- lubridate::month(df[[event.date]])
  df$day <- lubridate::day(df[[event.date]])
  # Remove rows with identical latitude, longitude, year, month, and day
  df <- dplyr::distinct(df, .data[[latitude]], .data[[longitude]], year, month, day, .keep_all = TRUE)
  # Removes extra columns
  df <- df[ , -which(names(df) %in% c("year", "month", "day"))]
  return(df)

}
