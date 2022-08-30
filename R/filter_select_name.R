#' @title filter_select_name
#'
#' @description
#' This function filters a data frame for relevant results, based on the scientific name given.
#'
#' @details
#' This function requires packages base and dplyr.
#'
#' @param df Data frame with name column to be fixed.
#' @param synonyms_list A list of synonyms for a species.
#' @param filter The type of filter to be used--either "exact", "fuzzy", or "interactive".
#' @param accepted_name The accepted scientific name for the species.
#'
#' @return Returns data frame with filtered results.
#'
#' @export
#'
#' @importFrom dplyr filter mutate

filter_select_name <- function(df, synonyms_list, filter, accepted_name) {
  print(paste0("Current scientific names ", unique(df$scientificName)))
  print(paste0("User selected a ", filter, "match"))
  if (filter == "interactive") {
    print("List of scientific names in the data set: ")
    print(unique(df$scientificName))

    input <- readline(prompt = "Would you like to remove any records from the data set? Enter Y for yes or N for no. ")
    while (input == "Y" | input == "y") {
      type <- readline(prompt = "Enter the scientific name to remove exactly as it is written. ")
      df <- df %>%
        dplyr::filter(scientificName != type)
      input <- readline(prompt = "Would you like to remove any additional records based on scientific name? Enter Y for yes or N for no. ")
    }
  }
  if (filter == "exact") {
      new_df <- data.frame()
      for (i in 1:length(synonyms_list)) {
        taxa <- synonyms_list[i]
        df_taxa <- df[df$scientificName == taxa, ]
        new_df <- rbind(new_df, df_taxa)
      }
      print("Scientific names kept: ")
      print(unique(df$scientificName))
    }
  else if (filter == "fuzzy") {
    new_df <- data.frame()
    for (i in 1:length(synonyms_list)) {
      taxa <- synonyms_list[i]
      df_taxa <- df[agrepl(taxa, df$scientificName, ignore.case = TRUE), ]
      new_df <- rbind(new_df, df_taxa)
    }
    print("Scientific names kept: ")
    print(unique(df$scientificName))
  }

  if (accepted_name != "") {
    accepted_name <- gsub(accepted_name, pattern = "_", replacement = " ")
    new_df <- dplyr::mutate(new_df, new_name = accepted_name)
  }
  return(new_df)
}
