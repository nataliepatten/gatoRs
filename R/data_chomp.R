#' @title Subset Data - Get species, longitude, and latitude columns
#'
#' @description
#' The `data_chomp()` function "chomps" (subsets) a data frame of occurrence records to only
#' contain the following columns: "species", "longitude", and "latitude". After using this
#' function data will be ready for use in Maxent, for example.
#'
#' @details
#' This function requires the package dplyr.
#'
#' @param df Data frame of occurrence records returned from `gators_download()`.
#' @param accepted.name The accepted species name for the records.
#' @inheritParams correct_class
#'
#' @examples
#' chomped_data <- data_chomp(data, accepted.name = "Galax urceolata")
#'
#' @return Returns data frame with a subset of columns ready for downstream applications such as Maxent.
#'
#' @importFrom dplyr select
#' @export

data_chomp <- function(df, accepted.name = NA,
                       longitude = "longitude", latitude = "latitude") {

  if (is.na(accepted.name)) {
    stop("Value not provided for argument: accepted.name.")
  }
  df$species <- accepted.name
  df <- dplyr::select(df, species, .data[[longitude]], .data[[latitude]])

  return(df)
}
