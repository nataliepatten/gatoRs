#' @title gatoRs Download -  Filter iDigBio results by scientific name
#'
#' @description
#' The `filter_fix_names()` function filters a data frame for relevant results, based on the scientific name given.
#' Some downloaded results from iDigBio might contain occurrences of other species that have "notes" mentioning
#' the desired species. Hence, this function looks for relevant results that are actually occurrences of the
#' desired species.
#'
#' @details
#' This function requires no additional packages.
#'
#' @param df Data frame with name column to be fixed.
#' @param synonyms_list A list of synonyms for a species.
#' @param filter The type of filter to be used--either "exact" or "fuzzy".
#' @param accepted_name The accepted scientific name for the species.
#'
#' @examples
#' data <- filter_fix_names(data, c("Galax urceolata", "Galax aphylla"), filter = "exact")
#' data <- filter_fix_names(data, c("Galax urceolata", "Galax aphylla"), accepted_name = "Galax urceolata")
#'
#' @return Returns data frame with filtered results.
#'
#' @export

filter_fix_names <- function(df, synonyms_list, filter = "fuzzy", accepted_name) {
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

  return(new_df)
}
