#' @title Taxonomic Cleaning - Filter and resolve taxon names
#'
#' @description
#' The `taxa_clean()` function filters a data frame for relevant results, based on the scientific name given.
#' Filtering can be done with scripts by exact or fuzzy match. Or, for a more controlled approach, this function
#' provides interactive filtering by providing the user with prompts. The interactive method allows the user
#' to manually determine whether they wish to keep results containing certain scientific names.
#'
#' @details
#' This function requires packages dplyr and magrittr.
#'
#' @param df Data frame with name column to be fixed.
#' @param synonyms_list A list of synonyms for a species.
#' @param filter The type of filter to be used--either "exact", "fuzzy", or "interactive".
#' @param accepted_name The accepted scientific name for the species.
#'
#' @examples
#' data <- taxa_clean(data, c("Galax urceolata", "Galax aphylla"), filter = "exact")
#' data <- taxa_clean(data, c("Galax urceolata", "Galax aphylla"), accepted_name = "Galax urceolata")
#'
#' @return Returns data frame with filtered results.
#'
#' @export
#'
#' @importFrom dplyr filter mutate
#' @importFrom magrittr "%>%"

taxa_clean <- function(df, synonyms_list, filter = "fuzzy", accepted_name = NA) {
  message("Current scientific names: ")
  print(unique(df$scientificName))
  message("User selected a(n) ", filter, " match.")
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
  } else if (filter == "exact") {
      new_df <- data.frame()

      for (i in 1:length(synonyms_list)) {
          taxa <- synonyms_list[i]
          df_taxa <- df[df$scientificName == taxa, ]
          new_df <- rbind(new_df, df_taxa)
      }
      message("Scientific names kept: ")
      print(unique(df$scientificName))
  } else if (filter == "fuzzy") {
      new_df <- data.frame()

        for (i in 1:length(synonyms_list)) {
            taxa <- synonyms_list[i]
            df_taxa <- df[agrepl(taxa, df$scientificName, ignore.case = TRUE), ]
            new_df <- rbind(new_df, df_taxa)
        }
      message("Scientific names kept: ")
      print(unique(df$scientificName))
  } else {
    message("Filter option is not avaliable. Please choose 'fuzzy', 'exact', or 'interactive'.")
  }


   if (! is.na(accepted_name)) {
      accepted_name <- gsub(accepted_name, pattern = "_", replacement = " ")
      new_df <- dplyr::mutate(new_df, new_name = accepted_name)
   } else {
      message("No accepted_name indicated!")
   }

  return(new_df)
}
