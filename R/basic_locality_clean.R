#' @title Locality Cleaning - Remove missing and improbable coordinates
#'
#' @description
#' The `basic_locality_clean()` function cleans locality by removing missing or impossible coordinates and correcting precision.
#'
#'
#' @param df Data frame of occurrence records returned from `gators_download()`.
#' @param remove.zero Default = "TRUE". Indicates that points at (0.00, 0.00) should be removed.
#' @param round.to Indicates digits to round coordinates too. Coordinates should be round to match the coordinate uncertainty. Default = 2.
#'
#' @return Return a cleaned data frame.
#'
#'
#' @export

basic_locality_clean <- function(df, remove.zero = TRUE, round.to = 2) {
      # Remove records with missing latitude and longitude
      df <-   df[!is.na(df$longitude), ]
      df <-   df[!is.na(df$latitude), ]
      # Remove records with impossible latitude and longitude
      df <- df[!(df$longitude > 180), ]
      df <- df[!(df$latitude > 180), ]
      df <- df[!(df$longitude < -180), ]
      df <- df[!(df$latitude < -180), ]
      # Removes records where latitude or longitude equals zero
      if(remove.zero == TRUE){
        df <- df[!(df$longitude == 0), ]
        df <- df[!(df$latitude == 0), ]
      }
      # Round for precision
      df$latitude <- round(df$latitude, digits = round.to)
      df$longitude <- round(df$longitude, digits = round.to)
      # Return df
      return(df)
}
