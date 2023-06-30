#' @title Full Cleaning - Wrapper function to speed clean
#'
#' @description
#' The `full_clean()` function performs automated cleaning steps, including options for: removing
#' duplicate data points, checking locality precision, removing points with skewed coordinates,
#' removing plain zero records, removing records based on basis of record, and spatially thinning collection points.
#' This function also provides the option to interactively inspect and remove types of basis of record.
#'
#' @details
#' This function requires packages dplyr, magrittr, and raster.
#'
#' @param df Data frame of occurrence records.
#' @inheritParams correct_class
#' @inheritParams remove_duplicates
#' @inheritParams taxa_clean
#' @param basis.list A list of basis to keep. If a list is not supplied, this filter will not occur.
#' @inheritParams basis_clean
#' @inheritParams basic_locality_clean
#' @param remove.flagged Default = TRUE. An option to remove points with problematic locality information.
#' @param thin.points Default = TRUE. An option to spatially thin occurrence records.
#' @param one.point.per.pixel Default = TRUE. An option to only retain one point per pixel.
#' @inheritParams thin_points
#' @inheritParams one_point_per_pixel
#'
#' @examples
#' cleaned_data <- full_clean(data, synonyms.list = c("Galax urceolata", "Galax aphylla"),
#' digits = 3, basis.list = c("Preserved Specimen","Physical specimen"),
#' accepted.name = "Galax urceolata", remove.flagged = FALSE)
#'
#' @return df is a data frame with the cleaned data.
#'
#' @export

full_clean <- function(df, synonyms.list, event.date = "eventDate",
                       year = "year", month = "month", day = "day",
                       occ.id = "occurrenceID",
                       remove.NA.occ.id = FALSE, remove.NA.date = FALSE,
                       aggregator = "aggregator", id = "ID",
                       taxa.filter = "fuzzy", scientific.name = "scientificName",
                       accepted.name = NA, remove.zero = TRUE,
                       precision = TRUE, digits = 2, remove.skewed = TRUE,
                       basis.list = NA, basis.of.record = "basisOfRecord",
                       latitude = "latitude", longitude = "longitude",
                       remove.flagged = TRUE, thin.points = TRUE,
                       distance = 5, reps = 100,
                       one.point.per.pixel = TRUE, raster = NA, resolution = 0.5) {

  suppress_output(df <- remove_duplicates(df, event.date = event.date,
                                          aggregator = aggregator, id = id, occ.id = occ.id,
                                          year = year, month = month, day = day,
                                          remove.NA.occ.id = remove.NA.occ.id, remove.NA.date = remove.NA.date,
                                          remove.unparseable = TRUE))
  suppress_output(df <- taxa_clean(df = df,  synonyms.list = synonyms.list,
               taxa.filter = taxa.filter, scientific.name = scientific.name, accepted.name =  accepted.name))

  if(!any(is.na(basis.list))){
    suppress_output(df <- basis_clean(df, basis.list = basis.list, basis.of.record = basis.of.record))
  }

  suppress_output(df <- basic_locality_clean(df, latitude = latitude, longitude = longitude,
                                             remove.zero = remove.zero, precision = precision,
                                             digits = digits, remove.skewed = remove.skewed))

  if (remove.flagged != TRUE & remove.flagged != FALSE) {
    # warning, since the rest of cleaning can occur even if this arg is invalid
    warning("Invalid value for argument: remove.flagged. Value for remove.flagged must equal 'TRUE' or 'FALSE'.")
  }
  else if (remove.flagged == TRUE) {
    suppress_output(df <- process_flagged(df, interactive = FALSE,
                                          latitude = latitude, longitude = longitude,
                                          scientific.name = scientific.name))
  }

  if (thin.points != TRUE & thin.points != FALSE) {
    # warning, since the rest of cleaning can occur even if this arg is invalid
    warning("Invalid value for argument: thin.points. Value for thin.points must equal 'TRUE' or 'FALSE'.")
  }
  else if (thin.points == TRUE) {
   suppress_output(df <- thin_points(df, accepted.name = accepted.name, distance = distance, reps = reps,
                                     latitude = latitude, longitude = longitude))
  }

  if(one.point.per.pixel == TRUE){
    suppress_output(df <- one_point_per_pixel(df, raster = raster, resolution = resolution,
                                              longitude = longitude, latitude = latitude))
  } else{
    df <- df
  }

  return(df)
}
