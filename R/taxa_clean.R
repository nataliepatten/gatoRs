#' @title Taxonomic Cleaning - Filter and resolve taxon names
#'
#' @description
#' The `taxa_clean()` function filters a data frame for relevant results, based on the scientific name given.
#' Filtering can be done with scripts by exact or fuzzy match. Or, for a more controlled approach, this function
#' provides interactive filtering by providing the user with prompts. The interactive method allows the user
#' to manually determine whether they wish to keep results containing certain scientific names.
#'
#' @details
#' If users select the interactive approach, the function will first print all unique scientific names
#' in the current data set and then ask the user to respond in the console to prompts regarding which records,
#' if any, should be removed based on their scientific name.
#'  After filtering, based on a user-provided taxonomy,  an accepted name column can be defined with an optional argument.
#'  This function relies on the user-provided taxonomy, we do not utilize any taxonomic backbone.
#'  Additionally, this function requires no additional packages.
#'
#' @param df Data frame of occurrence records returned from `gators_download()`.
#' @param synonyms.list A list of synonyms for a species.
#' @param taxa.filter The type of filter to be used--either "exact", "fuzzy", or "interactive".
#' @inheritParams correct_class
#' @param accepted.name The accepted scientific name for the species. If provided, an additional column will be added to the data frame with the accepted name for further manual comparison.
#'
#' @examples
#' cleaned_data <- taxa_clean(data, c("Galax urceolata", "Galax aphylla"), taxa.filter = "exact")
#' cleaned_data <- taxa_clean(data, c("Galax urceolata", "Galax aphylla"),
#' accepted.name = "Galax urceolata")
#'
#' @return Returns data frame with filtered results and new column with the accepted name labeled as "accepted_name".
#'
#' @export

taxa_clean <- function(df, synonyms.list, taxa.filter = "fuzzy", scientific.name = "scientificName", accepted.name = NA) {
  if (NROW(df) == 0) return(df)

  if (length(synonyms.list) == 0 | any(is.na(synonyms.list))) {
    stop("Invalid argument: synonyms.list. The argument synonyms.list must be non-empty.")
  }
  if (taxa.filter != "fuzzy" & taxa.filter != "exact" & taxa.filter != "interactive") {
    stop("Invalid value for argument: taxa.filter. Value for taxa.filter must equal 'fuzzy' or 'exact' or 'interactive'.")
  }

  message("Current scientific names: ")
  print(unique(df[[scientific.name]]))
  message("User selected a(n) ", taxa.filter, " match.")

  if (taxa.filter == "interactive") {
      input <- readline(prompt = "Would you like to remove any records from the data set? Enter Y for yes or N for no. ")
      new_df <- df
      while (input == "Y" | input == "y") {
          type <- readline(prompt = "Enter the scientific name to remove exactly as it is written. ")
          new_df <- new_df[new_df[[scientific.name]] != as.character(type), ]
          message("Scientific names kept: ")
          print(unique(new_df[[scientific.name]]))
          input <- readline(prompt = "Would you like to remove any additional records based on scientific name? Enter Y for yes or N for no. ")
      }
  } else if (taxa.filter == "exact") {
      old_df <- df
      new_df <- data.frame()
      for (i in 1:length(synonyms.list)) {
          taxa <- synonyms.list[i]
          df_taxa <- old_df[old_df[[scientific.name]] == taxa, ]
          old_df <- old_df[-(which(old_df[[scientific.name]] == taxa)), ]
          new_df <- rbind(new_df, df_taxa)
      }
  } else if (taxa.filter == "fuzzy") {
      old_df <- df
      new_df <- data.frame()
        for (i in 1:length(synonyms.list)) {
            taxa <- synonyms.list[i]
            df_taxa <- old_df[agrepl(taxa, old_df[[scientific.name]], ignore.case = TRUE), ]
            if(nrow(df_taxa) > 0){
              old_df <- old_df[-(which(old_df[[scientific.name]] %in% unique(df_taxa[[scientific.name]]))),]
            }
            new_df <- rbind(new_df, df_taxa)
        }
  } else {
    message("Filter option is not avaliable. Please choose 'fuzzy', 'exact', or 'interactive'.")
  }


   if (!is.na(accepted.name)) {
      accepted.name <- gsub(accepted.name, pattern = "_", replacement = " ")
      if (NROW(new_df) > 0) {
        new_df$accepted_name <- accepted.name
      }
   }

  return(new_df)
}
