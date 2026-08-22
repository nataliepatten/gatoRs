#' @title Basis Cleaning - Removes records with certain record basis
#'
#' @description
#' The `basis_clean()` function removes records based on [basisOfRecord](http://rs.tdwg.org/dwc/terms/basisOfRecord) column.
#'
#' @details
#' With the interactive method, the function will print all unique basisOfRecord values
#' in the current data set and then ask the user to respond in the console to prompts
#' regarding which records, if any, should be removed based on their basisOfRecord.
#' This function requires no additional packages.
#'
#' @param df Data frame of occurrence records returned from `gators_download()`.
#' @param basis.list A list of basis to keep. If a list is not supplied, the filter will be interactive and users must respond to the function.
#' @inheritParams correct_class
#'
#' @examples
#' cleaned_data <- basis_clean(data, basis.list = c("Preserved Specimen","Physical specimen"))
#'
#' @return Returns a data frame with records of desired record basis.
#' Information about the columns in the returned data frame can be found in the documentation for `gators_download()`.
#'
#' @export


basis_clean <- function(df, basis.list = NA, basis.of.record = "basisOfRecord"){
  if (NROW(df) == 0) return(df)

  if(any(is.na(basis.list)) | length(basis.list) == 0){
    record.filter <- "interactive"
  } else{
    record.filter <- "list-based"
  }

  if (record.filter == "interactive" ) {
    # interactive method for removal of basis of records
    message("Types of basis of records: ")
    print(unique(df[[basis.of.record]]))

    input <- readline(prompt = "Would you like to remove any types of basis of records? Enter Y for yes or N for no. ")
    new_df <- df
    while (input == "Y" | input == "y") {
      type <- readline(prompt = "Enter the type to remove exactly as it is written. ")
      new_df <- new_df[new_df[[basis.of.record]] != type, ]
      message("Basis of records kept: ")
      print(unique(new_df[[basis.of.record]]))
      input <- readline(prompt = "Would you like to remove any additional types of basis of records? Enter Y for yes or N for no. ")
    }

  } else if(record.filter == "list-based"){
    old_df <- df
    new_df <- data.frame()
    for (i in 1:length(basis.list)) {
      type <- basis.list[i]
      df_type <- old_df[agrepl(type, old_df[[basis.of.record]], ignore.case = TRUE), ]
      if(nrow(df_type) > 0){
        old_df <- old_df[-(which(old_df[[basis.of.record]] %in% unique(df_type[[basis.of.record]]))),]
      }
      new_df <- rbind(new_df, df_type)
    }
  }

  return(new_df)
}
