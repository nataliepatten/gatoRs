#' @title Filter data frame for relevant results
#'
#' @description
#' The `filter_select_name()` function filters a data frame for relevant results, based on the scientific name given.
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
#' data %>% filter_select_name(c("Asclepias curtissii", "Asclepias aceratoides", "Asclepias arenicola", "Oxypteryx arenicola", "Oxypteryx curtissii"), filter = "exact")
#' data %>% filter_select_name(c("Asclepias curtissii", "Asclepias aceratoides", "Asclepias arenicola", "Oxypteryx arenicola", "Oxypteryx curtissii"), accepted_name = "Asclepias curtissii")
#'
#' @return Returns data frame with filtered results.
#'
#' @export
#'
#' @importFrom dplyr filter mutate
#' @importFrom magrittr "%>%"

filter_select_name <- function(df, synonyms_list, filter = "fuzzy", accepted_name) {
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
  } else if (filter == "exact") {
      new_df <- data.frame()

      for (i in 1:length(synonyms_list)) {
          taxa <- synonyms_list[i]
          df_taxa <- df[df$scientificName == taxa, ]
          new_df <- rbind(new_df, df_taxa)
      }
      print("Scientific names kept: ")
      print(unique(df$scientificName))
  } else if (filter == "fuzzy") {
      new_df <- data.frame()

        for (i in 1:length(synonyms_list)) {
            taxa <- synonyms_list[i]
            df_taxa <- df[agrepl(taxa, df$scientificName, ignore.case = TRUE), ]
            new_df <- rbind(new_df, df_taxa)
        }
      print("Scientific names kept: ")
      print(unique(df$scientificName))
  } else {
    print("Filter option is not avaliable")
  }


   if (accepted_name != "") {
      accepted_name <- gsub(accepted_name, pattern = "_", replacement = " ")
      new_df <- dplyr::mutate(new_df, new_name = accepted_name)
   } else {
      print("No accepted_name indicated!")
   }

  return(new_df)
}
