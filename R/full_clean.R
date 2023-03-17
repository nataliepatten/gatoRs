#' @title Full Clean - Wrapper function to speed clean
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
#' @inheritParams taxa_clean
#' @inheritParams basic_locality_clean
#' @param basis.list A list of basis to keep. If a list is not supplied, this filter will not occur.
#'
#' @examples
#' data <- final_clean(data, recordBasis = FALSE)
#'
#' @return df is a data frame with the cleaned data.
#'
#'
#'
#' @export

final_clean <- function(df,
                        synonyms_list, taxa.filter = "fuzzy", accepted_name,
                        remove.zero = TRUE, precision = 2, remove.skewed = TRUE,
                        basis.list,
                        cluster = TRUE, ) {
  df <- suppressMessages(taxa_clean(df = df,  synonyms_list = synonyms_list, taxa.filter = taxa.filter, accepted_name =  accepted_name ))
  df <- basic_locality_clean(df, remove.zero = remove.zero, precision = precision,
                             remove.skewed = remove.skewed)
  df <- remove_duplicates(df)
  if(length(basis.list) > 1){
  df <- basis_clean(df, basis.list = basis.list)
  }

}
