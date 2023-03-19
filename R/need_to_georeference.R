#' @title Identify Missing Information - Find records which lack coordinate information
#'
#' @description
#' The `need_to_georeference()` function allows you to find records that are missing coordinates
#' but contain locality information. These records can then be manually georeferenced.
#'
#'
#' @param df A data frame downloaded with `gators_download()`.
#'
#' @examples
#' need_coords <- need_to_georeference(data)
#'
#' @return Returns a data frame of the points that need to be georeferenced.
#' For more information about this data frame, see `gators_download()`.
#'
#'
#' @export


need_to_georeference <- function(df){
  if (NROW(df) == 0) return(df)

  for_georeferencing <- df
  # ID records with missing latitude and longitude
  for_georeferencing  <- for_georeferencing[is.na(for_georeferencing$longitude), ]
  for_georeferencing  <- for_georeferencing[is.na(for_georeferencing$latitude), ]
  # ID records with locality information included
  for_georeferencing <- for_georeferencing[!is.na(for_georeferencing$locality), ]
  for_georeferencing <- for_georeferencing[!grepl("locality:  NA, occurrenceRemarks: NA, verbatimLocality: NA", for_georeferencing$locality), ]

  return(for_georeferencing)
}
