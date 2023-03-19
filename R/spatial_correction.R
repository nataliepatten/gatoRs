#' @title Spatial Correction -
#'
#' @description
#' The `spatial_correction` function performs coordinate thinning.
#'
#' @details
#' This function requires package spThin.
#'
#' @param df Data frame of occurrence records.
#' @param accepted.name Accepted name of your species. This argument is not required
#' if the data frame already contains an accepted_name column.
#' @param thin.par Default = 1000. Distance in km to separate records.
#' @param reps Default = 1. Number of times to perform thinning.
#'
#' @examples
#' \dontrun{
#' data <- spatial_correction(data, accepted.name = "Galax urceolata")
#' }
#'
#' @return df is a data frame with the cleaned data.
#'
#' @importFrom spThin thin
#'
#' @keywords internal
#' @export

spatial_correction <- function(df, accepted.name = NA, thin.par = 1000, reps = 1) {
  if (is.na(accepted.name) & !("accepted_name" %in% colnames(df))) {
    stop("Error: The data frame does not already have an accepted_name column. \nPlease provide a value for argument accepted.name.")
  }
  else if (is.na(accepted.name) & ("accepted_name" %in% colnames(df))) {}
  else {
    df$accepted_name <- accepted.name
  }

  thin_data <- spThin::thin(df, lat.col="latitude", long.col = "longitude",
                     spec.col = "accepted_name", thin.par = thin.par,
                     reps = reps, write.files = FALSE, write.log.file = FALSE,
                     locs.thinned.list.return = TRUE)
  thin_data <- thin_data[[1]]
  df <- df[df$latitude == thin_data$Latitude & df$longitude == thin_data$Longitude, ]

  return(df)
}
