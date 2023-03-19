#' @title Full Cleaning - Wrapper function to speed clean
#'
#' @description
#' The `full_clean()` function performs automated cleaning steps, including options for: removing
#' duplicate data points, checking locality precision, removing points with skewed coordinates,
#' removing plain zero records, removing records based on basis of record, and spatially thinning collection points.
#' This function also provides the option to interactively inspect and remove types of basis of record.
#'
#' @details
#' This function requires packages dplyr, magrittr, raster, and dismo.
#'
#' @param df Data frame of occurrence records.
#' @inheritParams taxa_clean
#' @inheritParams basic_locality_clean
#' @param basis.list A list of basis to keep. If a list is not supplied, this filter will not occur.
#' @param remove.flagged Default = TRUE. An option to remove points with problematic locality information.
#' @param thin.points Default = TRUE. An option to spatially thin occurrence records.
#' @inheritParams thin_points
#'
#' @examples
#' data <- full_clean(data, synonyms.list = c("Galax urceolata", "Galax aphylla"), digits = 3, basis.list = "HUMAN_OBSERVATION", accepted.name = "Galax urceolata")
#' data <- full_clean(data, synonyms.list = "Galax urceolata", remove.skewed = FALSE, remove.flagged = FALSE, thin.points = FALSE)
#'
#' @return df is a data frame with the cleaned data.
#'
#' @export

full_clean <- function(df, synonyms.list, taxa.filter = "fuzzy",
                        accepted.name = NA, remove.zero = TRUE,
                        precision = TRUE, digits = 2, remove.skewed = TRUE,
                        basis.list = NA, remove.flagged = TRUE, thin.points = TRUE, distance = 5, reps = 100) {

  suppress_output(df <- remove_duplicates(df))
  suppress_output(df <- taxa_clean(df = df,  synonyms.list = synonyms.list,
               taxa.filter = taxa.filter, accepted.name =  accepted.name))

  if(!is.na(basis.list)){
    suppress_output(df <- basis_clean(df, basis.list = basis.list))
  }

  suppress_output(df <- basic_locality_clean(df, remove.zero = remove.zero, precision = precision,
                             digits = digits, remove.skewed = remove.skewed))

  if (remove.flagged != TRUE & remove.flagged != FALSE) {
    # warning, since the rest of cleaning can occur even if this arg is invalid
    warning("Invalid value for argument: remove.flagged. Value for remove.flagged must equal 'TRUE' or 'FALSE'.")
  }
  else if (remove.flagged == TRUE) {
    suppress_output(df <- process_flagged(df, interactive = FALSE))
  }

  if (thin.points != TRUE & thin.points != FALSE) {
    # warning, since the rest of cleaning can occur even if this arg is invalid
    warning("Invalid value for argument: thin.points. Value for thin.points must equal 'TRUE' or 'FALSE'.")
  }
  else if (thin.points == TRUE) {
   suppress_output(df <- thin_points(df, accepted.name = accepted.name, distance = distance, reps = reps))
  }

  return(df)
}
