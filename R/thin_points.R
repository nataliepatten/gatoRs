#' @title Spatial Correction - Spatially thin records
#'
#' @description
#' The `thin_points` function returns records based on coordinate thinning.
#'
#' @details
#' This function requires package spThin.
#'
#' @param df Data frame of occurrence records.
#' @param accepted.name Accepted name of your species. This argument is not required
#' if the data frame already contains an accepted_name column.
#' @param distance Default = 1000. Distance in km to separate records.
#' @param reps Default = 1. Number of times to perform thinning algorithm.
#'
#' @examples
#' data <- thin_points(data, accepted.name = "Galax urceolata")
#'
#' @return df is a data frame with the cleaned data.
#'
#' @importFrom spThin thin
#'
#' @export

thin_points <- function(df, accepted.name = NA, distance = 5, reps = 100) {
  if (is.na(accepted.name) & !("accepted_name" %in% colnames(df))) {
    stop("Error: The data frame does not already have an accepted_name column. \nPlease provide a value for argument accepted.name.")
  }
  else if (is.na(accepted.name) & ("accepted_name" %in% colnames(df))) {}
  else {
    df$accepted_name <- accepted.name
  }

  thin_data <- spThin::thin(df, lat.col="latitude", long.col = "longitude",
                     spec.col = "accepted_name", thin.par = distance,
                     reps = reps, write.files = FALSE, write.log.file = FALSE,
                     locs.thinned.list.return = TRUE)
  thin_data <- thin_data[[1]]
  df <- df[df$latitude %in% thin_data$Latitude & df$longitude %in% thin_data$Longitude, ]

  return(df)
}
