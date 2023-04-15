#' @title Subset Data - Get species, longitude, and latitude columns
#'
#' @description
#' The `data_chomp()` function "chomps" (subsets) a data frame of occurrence records to only
#' contain the following columns: "species", "longitude", and "latitude". After using this
#' function data will be ready for use in Maxent, for example.
#'
#' @details
#' This function requires no additional packages.
#'
#' @param df Data frame of occurrence records returned from `gators_download()`.
#' @inheritParams correct_class
#'
#' @examples
#' chomped_data <- data_chomp(data)
#'
#' @return Returns data frame with a subset of columns ready for downstream applications such as Maxent.
#'
#' @export

data_chomp <- function(df, scientific.name = "scientificName",
                       longitude = "longitude", latitude = "latitude") {
  df <- df[, which(names(df) %in% c(scientific.name, longitude, latitude))]
  colnames(df)[1] <- "species"
  return(df)
}
