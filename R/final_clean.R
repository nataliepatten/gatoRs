#' @title Perform final cleaning of data
#'
#' @description
#' The `final_clean()` function performs final cleaning steps, including: removing
#' duplicate data points, checking locality precision, and retaining only one collection point.
#' This function also provides the option to interactively inspect and remove types of basis of record.
#'
#' @details
#' This function requires packages dplyr, magrittr, raster, and dismo.
#'
#' @param df Data frame of occurrence records.
#' @param cluster An option (TRUE/FALSE) to cluster geographically close occurrence records.
#' This option is turned on by default.
#' @param precision An option to round coordinates to the specified number of digits.
#' This option is set to 2 by default. To not use any rounding, choose `precision = NA`.
#' @param recordBasis An option (TRUE/FALSE) to interactively remove types of basis of record.
#' This option is turned on by default.
#'
#' @examples
#' data %>% final_clean(recordBasis = FALSE)
#'
#' @return df is a data frame with the cleaned data.
#'
#'
#' @importFrom raster raster
#' @importFrom dismo gridSample
#' @importFrom magrittr "%>%"
#'
#' @export

final_clean <- function(df, cluster = TRUE, precision = 2, recordBasis = TRUE) {

  # round for precision
  if (!is.na(precision)) {
    df$latitude <- round(df$latitude, digits = precision)
    df$longitude <- round(df$longitude, digits = precision)

   # bio1 <- raster::raster("data/CLIMATE/bio1.bil")

    # out <- dismo::gridSample(df, bio1, n = 1) # add to function
  }


  # remove duplicates
  df <- dplyr::distinct(df, longitude, latitude, .keep_all = TRUE)

  if (recordBasis) {
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
  }
  return(df)
}
