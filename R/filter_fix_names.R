#' @title Used in gators_download() -  Filter iDigBio results by scientific name
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
#' @param synonyms.list A list of synonyms for a species.
#' @param filter Default = "fuzzy". Indicates the type of filter to be used--either "exact" or "fuzzy".
#' @param accepted.name The accepted scientific name for the species. If provided, an additional column will be added to the data frame with the accepted name for further manual comparison.
#'
#' @examples
#' clean_data <- filter_fix_names(data, c("Galax urceolata", "Galax aphylla"), filter = "exact")
#' clean_data <- filter_fix_names(data, c("Galax urceolata", "Galax aphylla"), accepted.name = "Galax urceolata")
#'
#' @return Returns data frame with filtered results.
#'
#' @export

filter_fix_names <- function(df, synonyms.list, filter = "fuzzy", accepted.name = NA) {
  if (NROW(df) == 0) return(df)

  if (!("scientificName" %in% colnames(df))) {
    stop("Missing column 'scientificName'.")
  }

  if (length(synonyms.list) == 0 | any(is.na(synonyms.list))) {
    stop("Invalid argument: synonyms.list. The argument synonyms.list must be non-empty.")
  }

  if (filter != "fuzzy" & filter != "exact") {
    stop("Invalid value for argument: filter. Value for filter must equal 'fuzzy' or 'exact'.")
  }

  if (filter == "exact") {
    new_df <- data.frame()
    for (i in 1:length(synonyms.list)) {
      taxa <- synonyms.list[i]
      df_taxa <- df[df$scientificName == taxa, ]
      new_df <- rbind(new_df, df_taxa)
    }
  }
  else if (filter == "fuzzy") {
    new_df <- data.frame()
    for (i in 1:length(synonyms.list)) {
      taxa <- synonyms.list[i]
      df_taxa <- df[agrepl(taxa, df$scientificName, ignore.case = TRUE), ]
      new_df <- rbind(new_df, df_taxa)
    }
  }

  if (!is.na(accepted.name)) {
    accepted.name <- gsub(accepted.name, pattern = "_", replacement = " ")
    new_df$accepted_name <- accepted.name
  }
  return(new_df)
}
