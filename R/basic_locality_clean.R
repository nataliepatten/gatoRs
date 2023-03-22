#' @title Locality Cleaning - Remove missing and improbable coordinates
#'
#' @description
#' The `basic_locality_clean()` function cleans locality by removing missing or impossible coordinates and correcting precision.
#' This function requires columns named 'latitude' and 'longitude'. These columns should be of type 'numeric'.
#'
#' @param df Data frame of occurrence records returned from `gators_download()`.
#' @param latitude Default = "latitude". The name of the latitude column in the data frame.
#' @param longitude Default = "longitude". The name of the longitude column in the data frame.
#' @param remove.zero Default = TRUE. Indicates that points at (0.00, 0.00) should be removed.
#' @param precision Default = TRUE. Indicates that coordinates should be rounded to match the coordinate uncertainty.
#' @param digits Default = 2. Indicates digits to round coordinates to when `precision = TRUE`.
#' @param remove.skewed Default = TRUE. Utilizes the `remove_skewed()` function to remove skewed coordinate values.

#' @examples
#' data <- basic_locality_clean(data)
#'
#' @return Return data frame with specimen removed that had missing or improper coordinate values.
#'
#' @export

basic_locality_clean <- function(df, latitude = "latitude", longitude = "longitude", remove.zero = TRUE,
                                 precision = TRUE, digits = 2, remove.skewed = TRUE) {
  if (NROW(df) == 0) return(df)

  if (!(longitude %in% colnames(df))) {
    stop("Missing column ", longitude, ".")
  }
  if (!(latitude %in% colnames(df))) {
    stop("Missing column ", latitude, ".")
  }
  if (!is.numeric(df[[longitude]]) || !is.numeric(df[[latitude]])) {
    stop("Columns ", latitude, " and ", longitude, " must be of type 'numeric'.")
  }
  if (precision != TRUE & precision != FALSE) {
    stop("Invalid value for argument: precision. Value for precision must equal 'TRUE' or 'FALSE'.")
  }
  if (remove.skewed != TRUE & remove.skewed != FALSE) {
    stop("Invalid value for argument: remove.skewed. Value for remove.skewed must equal 'TRUE' or 'FALSE'.")
  }
  if (precision & !is.numeric(digits)) {
    stop("Invalid value for argument: digits. Value for digits must be numerical.")
  }

  # Remove records with missing latitude and longitude
  df <- df[!is.na(df[[longitude]]), ]
  df <- df[!is.na(df[[latitude]]), ]
  # Remove records with impossible latitude and longitude
  df <- df[!(df[[longitude]] > 180), ]
  df <- df[!(df[[latitude]] > 180), ]
  df <- df[!(df[[longitude]] < -180), ]
  df <- df[!(df[[latitude]] < -180), ]
  # Removes records where latitude or longitude equals zero
  if(remove.zero == TRUE){
    df <- df[!(df[[longitude]] == 0), ]
    df <- df[!(df[[latitude]] == 0), ]
  }
  # Round for precision
  if(precision){
    df[[latitude]] <- round(df[[latitude]], digits = digits)
    df[[longitude]] <- round(df[[longitude]], digits = digits)
  }
  # Remove skewed
  if(remove.skewed){
   df <- remove_skewed(df)
  }
  # Return df
  return(df)
}
