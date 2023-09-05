#' @title Spatial Correction - Spatially thin records
#'
#' @description
#' The `thin_points` function returns records based on coordinate thinning based on a minimum nearest neighbor distance approach.
#'
#' @details
#' This function is a wrapper for spatial thinning using the spThin package (Aiello-Lammens et al., 2015)
#' In summary, the thinning algorithm provided by spThin calculates the pairwise distances between data points,
#' then randomly samples a single point from all points less than or equal to the set minimum nearest neighbor distance.
#' This process is repeated until the pairwise distances among points do not fall below the minimum nearest neighbor distance.
#'
#' @param df Data frame of occurrence records.
#' @param accepted.name Accepted name of your species. This argument is not required
#' if the data frame already contains an accepted_name column.
#' @param distance Default = 5. Distance in km to separate records.
#' @param reps Default = 100. Number of times to perform thinning algorithm.
#' @inheritParams correct_class
#'
#' @examples
#' thinned_data <- thin_points(data, accepted.name = "Galax urceolata")
#'
#' @return df is a data frame with the cleaned data.
#'
#' @importFrom spThin thin
#'
#' @export

thin_points <- function(df, accepted.name = NA, distance = 5, reps = 100, latitude = "latitude", longitude = "longitude") {
  if (NROW(df) == 0) return(df)

  if (is.na(accepted.name) & !("accepted_name" %in% colnames(df))) {
    stop("The data frame does not already have an accepted_name column. \nPlease provide a value for argument accepted.name.")
  }
  else if (is.na(accepted.name) & ("accepted_name" %in% colnames(df))) {}
  else {
    df$accepted_name <- accepted.name
  }

  suppress_output(
    thin_data <- spThin::thin(df, lat.col=latitude, long.col = longitude,
                     spec.col = "accepted_name", thin.par = distance,
                     reps = reps, write.files = FALSE, write.log.file = FALSE,
                     locs.thinned.list.return = TRUE))
  thin_data <- thin_data[[reps]]
  df <- df[df[[latitude]] %in% thin_data$Latitude & df[[longitude]] %in% thin_data$Longitude, ]

  return(df)
}
