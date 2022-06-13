#' @title final_clean
#'
#' @description
#' This function performs final cleaning steps, including: removing
#' duplicate data points, checking locality precision, and
#' retaining only one collection point.
#' @details
#' This function requires packages dplyr, base, raster, spatstat
#'
#' @param df is a dataframe of occurrence records
#'
#' @return df is a dataframe with the cleaned data
#'
#' @export

final_clean <- function(df) {
  # one point per pixel
  bio1 <- raster::raster("data/CLIMATE/bio1.bil")
  rasterResolution <- max(res(bio1))
  while(min(spatstat::nndist(df[,2:3])) < rasterResolution){
    nnD <- spatstat::nndist(df[,2:3])
    df <- df[-(which(min(nnD) == nnD)[1]),]
  }

  # round for precision
  df$Latitude <- round(df$Latitude, digits = 2)
  df$Longitude <- round(df$Longitude, digits = 2)

  # remove duplicates
  df <- dplyr::distinct(df, Longitude, Latitude, .keep_all = TRUE)

  # interactive method for removal of basis of records
  print("Types of basis of records: ")
  print(unique(df$basisOfRecord))

  input <- readline(prompt = "Would you like to remove any types of basis of records? Enter Y for yes or N for no. ")

  while (input == "Y" | input == "y") {
    type <- readline(prompt = "Enter the type to remove exactly as it is written. ")
    df <- df %>%
      dplyr::filter(basisOfRecord != type)
    input <- readline(prompt = "Would you like to remove any additional types of basis of records? Enter Y for yes or N for no. ")
  }
  return(df)
}
