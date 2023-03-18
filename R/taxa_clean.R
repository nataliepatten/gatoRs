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
#' @param df Data frame of occurrence records returned from `gators_download()`.
#' @param synonyms.list A list of synonyms for a species.
#' @param taxa.filter The type of filter to be used--either "exact", "fuzzy", or "interactive".
#' @param accepted.name The accepted scientific name for the species. If provided, an additional column will be added to the data frame with the accepted name for further manual comparison.
#'
#' @examples
#' clean_data <- taxa_clean(data, c("Galax urceolata", "Galax aphylla"), taxa.filter = "exact")
#' clean_data <- taxa_clean(data, c("Galax urceolata", "Galax aphylla"), accepted.name = "Galax urceolata")
#'
#' @return Returns data frame with filtered results and new column with the accepted name labeled as "accepted_name".
#'
#' @export
#'
#' @importFrom dplyr filter mutate
#' @importFrom magrittr "%>%"

taxa_clean <- function(df, synonyms.list, taxa.filter = "fuzzy", accepted.name = NA) {
  if (NROW(df) == 0) return(df)

  if (length(synonyms.list) == 0 | any(is.na(synonyms.list))) {
    stop("Invalid argument: synonyms.list. The argument synonyms.list must be non-empty.")
  }
  if (taxa.filter != "fuzzy" & taxa.filter != "exact" & taxa.filter != "interactive") {
    stop("Invalid value for argument: taxa.filter. Value for taxa.filter must equal 'fuzzy' or 'exact' or 'interactive'.")
  }

  message("Current scientific names: ")
  print(unique(df$scientificName))
  message("User selected a(n) ", taxa.filter, " match.")

  if (taxa.filter == "interactive") {
      message("List of scientific names in the data set: ")
      print(unique(df$scientificName))
      input <- readline(prompt = "Would you like to remove any records from the data set? Enter Y for yes or N for no. ")
      new_df <- df
      while (input == "Y" | input == "y") {
          type <- readline(prompt = "Enter the scientific name to remove exactly as it is written. ")
          new_df <- new_df[new_df$scientificName != as.character(type), ]
          message("Scientific names kept: ")
          print(unique(new_df$scientificName))
          input <- readline(prompt = "Would you like to remove any additional records based on scientific name? Enter Y for yes or N for no. ")
      }
  } else if (taxa.filter == "exact") {
      new_df <- data.frame()

      for (i in 1:length(synonyms.list)) {
          taxa <- synonyms.list[i]
          df_taxa <- df[df$scientificName == taxa, ]
          new_df <- rbind(new_df, df_taxa)

      }
      message("Scientific names kept: ")
      print(unique(new_df$scientificName))
  } else if (taxa.filter == "fuzzy") {
      new_df <- data.frame()

        for (i in 1:length(synonyms.list)) {
            taxa <- synonyms.list[i]
            df_taxa <- df[agrepl(taxa, df$scientificName, ignore.case = TRUE), ]
            new_df <- rbind(new_df, df_taxa)
        }
      message("Scientific names kept: ")
      print(unique(new_df$scientificName))
  } else {
    message("Filter option is not avaliable. Please choose 'fuzzy', 'exact', or 'interactive'.")
  }


   if (!is.na(accepted.name)) {
      accepted.name <- gsub(accepted.name, pattern = "_", replacement = " ")
      new_df$accepted_name <- accepted.name
   }

  return(new_df)
}
