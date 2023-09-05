#' @title Remove Duplicates - Remove records with identical event dates and coordinates
#'
#' @description
#' The `remove_duplicates()` function removes records with identical event dates and occurrence IDs.
#' Prior to utilizing this function, longitude and latitude columns should be rounded to match the
#' coordinate uncertainty using the `basic_locality_clean()` function.
#'
#' @details
#' Here we identify and remove both (1) specimen duplicates and (2) aggregator duplicates based on each specimens coordinates,
#' occurrenceID, and eventDate. To leverage all date information
#' available, set `remove.unparseable = FALSE` to manually populate
#' the year, month, and day columnsl. Dates are parsed based on ISO 8601 which only includes time since the Unix epoch, or January 1st, 1970, therefore dates that occur
#' before 1970 will not be automatically parsed.  If we are unable to parse the included date for particular records,
#' users can choose to manually enter the year, month, and day for these records when prompted.
#' If the user chooses to manually enter the event date, the records eventDate will be printed
#' and the user will be asked to manually enter the year, month, and day of this eventDate into the console.
#' Users are only prompted to manually parse event dates for records where year, month, and day are absent,
#' but eventDate is present and cannot be parsed.
#' This function also we also confirm all ID (UUID and key) are unique to remove any within-aggregator duplicates that may accumulate due to processing errors.
#' This function requires the parsedate and dplyr packages. Warning, this function will ignore missing occurrence ID
#' and year, month, day columns if not provided in the data set.
#'
#' @param df Data frame of occurrence records returned from `gators_download()`.
#' @inheritParams correct_class
#' @param remove.NA.occ.id Default = FALSE. This will remove records with missing occurrence IDs when set to `TRUE`.
#' @param remove.NA.date Default = FALSE. This will remove records with missing event dates when set to `TRUE`.
#' @param remove.unparseable Default = FALSE. If we cannot parse the event date into individual year,
#' month, day categories the user can manually specify. Otherwise, if set to TRUE, these rows will simply be removed.
#'
#' @examples
#' cleaned_data <- remove_duplicates(data)
#' cleaned_data <- remove_duplicates(data, remove.NA.occ.id = TRUE, remove.NA.date = TRUE)
#' cleaned_data <- remove_duplicates(data, remove.unparseable = TRUE)
#'
#' @return Return data frame with duplicates removed.
#'
#' @importFrom parsedate parse_iso_8601 format_iso_8601
#' @importFrom dplyr distinct mutate select row_number filter
#' @importFrom stats na.omit
#'
#' @export

remove_duplicates <- function(df, event.date = "eventDate",
                              aggregator = "aggregator", id = "ID", occ.id = "occurrenceID",
                              year = "year", month = "month", day = "day",
                              latitude = "latitude", longitude = "longitude",
                              remove.NA.occ.id = FALSE, remove.NA.date = FALSE,
                              remove.unparseable = FALSE){

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
    tempdf[[i]] <- df[df[[aggregator]] == ag[i], ]
    if( (length(unique(stats::na.omit(tempdf[[i]][[id]])))) != nrow(stats::na.omit(tempdf[[i]][id])) ) {
      tempdf[[i]] <- dplyr::distinct(tempdf[[i]], .data[[id]], .keep_all = TRUE)
    }
  }
  df <- do.call(rbind, tempdf)

  # Remove specimen duplicates

  has_date_cols <- function(df, i) {
    # individual year, month, day columns don't even exist
    if (length(df[[year]]) == 0 & length(df[[month]]) == 0 & length(df[[day]]) == 0) {
      return(FALSE)
    } else if (is.na(df[[year]][i]) | is.na(df[[month]][i])) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }

  get_temp_date <- function(date, remove.unparseable) {

    tryCatch (
      return(as.character(parsedate::parse_iso_8601(parsedate::format_iso_8601(date)))),
      error=function(e) {
        message("Event date cannot be automatically parsed for date: ", date)
        if (remove.unparseable) {
          return("remove")
        } else {
          temp_message <- paste0("Date cannot be automatically parsed for the eventDate: ", date)
          temp_year <- readline(prompt = paste(temp_message, "Please enter the year in YYYY format, or NA if not provided: ", sep = "\n"))
          temp_month <- readline(prompt = "Please enter the month in MM format, or NA if not provided: ")
          temp_day <- readline(prompt = "Please enter the day in DD format or NA if not provided: ")
          temp_date <- paste(temp_year, temp_month, temp_day, sep="-")
          return(temp_date)
        }
      }
    )
  }

  new_df <- df
  to_remove <- c()
  for(i in 1:nrow(df)) {
    # If year, month, day are not available, but eventDate is, attempt to parse
    if(!is.na(df[[event.date]][i]) & !has_date_cols(df, i)) {
      temp_date <- get_temp_date(df[[event.date]][i], remove.unparseable)

      if (temp_date == "remove") {
        to_remove <- append(to_remove, i)
      } else {
        if (substr(temp_date, 1, 4) == "NA") {
          new_df[[year]][i] <- NA
        } else{
          new_df[[year]][i] <- substr(temp_date, 1, 4)
        }
        if (substr(temp_date, 6, 7) == "NA") {
          new_df[[month]][i] <- NA
        } else{
          new_df[[month]][i] <- substr(temp_date, 6, 7)
        }
        if (substr(temp_date, 9, 10) == "NA") {
          new_df[[day]][i] <- NA
        } else{
          new_df[[day]][i] <- substr(temp_date, 9, 10)
        }
      }
    } else {
      if (length(df[[year]]) == 0) {new_df[[year]][i] <- NA} else {new_df[[year]][i] <- df[[year]][i]}
      if (length(df[[month]]) == 0) {new_df[[month]][i] <- NA} else {new_df[[month]][i] <- df[[month]][i]}
      if (length(df[[day]]) == 0) {new_df[[day]][i] <- NA} else {new_df[[day]][i] <- df[[day]][i]}
    }
  }

  if (remove.unparseable == TRUE) {new_df <- dplyr::filter(new_df, !(row_number() %in% to_remove))}

  new_df[[year]] <- as.numeric(new_df[[year]])
  new_df[[month]] <- as.numeric(new_df[[month]])
  new_df[[day]] <- as.numeric(new_df[[day]])

  # If occurrence ID column exists, remove rows with identical occurrence ID and date
  if (length(df[[occ.id]]) == 0) {
    # Create a temporary column with unique values so NAs in dates are kept
    # https://stackoverflow.com/questions/66537554/keeping-all-nas-in-dplyr-distinct-function
    new_df <- dplyr::mutate(new_df, temp = dplyr::row_number() * (is.na(year) & is.na(month) & is.na(day)))
    new_df <- dplyr::distinct(new_df, .data[[year]], .data[[month]], .data[[day]], .data[[latitude]], .data[[longitude]], temp, .keep_all = TRUE)
    new_df <- dplyr::select(new_df, -temp)
  } else if (length(df[[occ.id]]) > 0) {
    new_df <- dplyr::mutate(new_df, temp = row_number() * (is.na(year) & is.na(month) & is.na(day)))
    new_df <- dplyr::distinct(new_df, .data[[year]], .data[[month]],
                              .data[[day]], .data[[occ.id]],  .data[[latitude]], .data[[longitude]], temp, .keep_all = TRUE)
    new_df <- dplyr::select(new_df, -temp)
  }

  return(new_df)
}
