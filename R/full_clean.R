#' @title Full Cleaning - Wrapper function to speed clean
#'
#' @description
#' The `full_clean()` function performs automated cleaning steps, including options for: removing
#' duplicate data points, checking locality precision, removing points with skewed coordinates,
#' removing plain zero records, removing records based on basis of record, and clustering collection points.
#' This function also provides the option to interactively inspect and remove types of basis of record.
#'
#' @details
#' This function requires packages dplyr, magrittr, raster, and dismo.
#'
#' @param df Data frame of occurrence records.
#' @inheritParams taxa_clean
#' @inheritParams basic_locality_clean
#' @param basis.list A list of basis to keep. If a list is not supplied, this filter will not occur.
#' @param cluster Default = TRUE. An option to cluster geographically close occurrence records.
#' @param remove.flagged Default = TRUE. An option to remove points with problematic locality information.
#'
#' @examples
#' data <- full_clean(data, synonyms.list = c("Galax urceolata", "Galax aphylla"), digits = 3, accepted.name = "Galax urceolata")
#' data <- full_clean(data, synonyms.list = "Galax urceolata", remove.skewed = FALSE, basis.list = "HUMAN_OBSERVATION", remove.flagged = FALSE, cluster = FALSE)
#'
#' @return df is a data frame with the cleaned data.
#'
#'
#'
#' @export

full_clean <- function(df, synonyms.list, taxa.filter = "fuzzy",
                        accepted.name = NA, remove.zero = TRUE,
                        precision = TRUE, digits = 2, remove.skewed = TRUE,
                        basis.list = NA, remove.flagged = TRUE, cluster = TRUE) {

  df <- remove_duplicates(df)
  df <- suppressMessages(taxa_clean(df = df,  synonyms.list = synonyms.list, taxa.filter = taxa.filter, accepted.name =  accepted.name ))

  if(!is.na(basis.list)){
    df <- basis_clean(df, basis.list = basis.list)
  }

  df <- basic_locality_clean(df, remove.zero = remove.zero, precision = precision,
                             digits = digits, remove.skewed = remove.skewed)

  if (remove.flagged != TRUE & cluster != FALSE) {
    # warning, since the rest of cleaning can occur even if this arg is invalid
    warning("Invalid value for argument: remove.flagged. Value for remove.flagged must equal 'TRUE' or 'FALSE'.")
  }
  else if (remove.flagged == TRUE) {
    df <- suppressMessages(process_flagged(df, interactive = FALSE))
  }

  if (cluster != TRUE & cluster != FALSE) {
    # warning, since the rest of cleaning can occur even if this arg is invalid
    warning("Invalid value for argument: cluster. Value for cluster must equal 'TRUE' or 'FALSE'.")
  }
  else if (cluster == TRUE) {
    # TO-DO: add cluster option
  }

  return(df)
}
