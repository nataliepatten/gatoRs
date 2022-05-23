#' @title filter_fix_names
#'
#' @description
#' This function filters a data frame for relevant results, based on the scientific name given.
#'
#' @details
#' This function requires packages base and dplyr.
#'
#' @param df is a data frame with name column to be fixed
#' @param synonyms_list is a list of synonyms for a species
#' @param filter is the type of filter to be used--either "exact" or "fuzzy"
#' @param accepted_name is the accepted scientific name for the species
#'
#' @return returns df with filtered results
#'
#' @importFrom dplyr mutate

filter_fix_names <- function(df, synonyms_list, filter, accepted_name) {
  if (filter == "exact") {
    new_df <- data.frame()
    for (i in 1:length(synonyms_list)) {
      taxa <- synonyms_list[i]
      df_taxa <- df[df$scientificName == taxa, ]
      new_df <- rbind(new_df, df_taxa)
    }
  }
  else if (filter == "fuzzy") {
    new_df <- data.frame()
    for (i in 1:length(synonyms_list)) {
      taxa <- synonyms_list[i]
      df_taxa <- df[agrepl(taxa, df$scientificName, ignore.case = TRUE), ]
      new_df <- rbind(new_df, df_taxa)
    }
  }

  if (accepted_name != "") {
    accepted_name <- gsub(accepted_name, pattern = "_", replacement = " ")
    new_df <- dplyr::mutate(new_df, new_name = accepted_name)
  }
  return(new_df)
}
