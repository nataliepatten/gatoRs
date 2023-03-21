#' @title Basis Cleaning - Removes records with certain record basis
#'
#' @description
#' The `basis_clean()` function removes records based on basisOfRecord column
#'
#' @param df Data frame of occurrence records returned from `gators_download()`.
#' @param basis.list A list of basis to keep. If a list is not supplied, the filter will be interactive and users must respond to the function.
#'
#' @examples
#' data <- basis_clean(data, basis.list = c("Preserved Specimen","Physical specimen"))
#'
#' @export


basis_clean <- function(df, basis.list = NA){
  if (NROW(df) == 0) return(df)

  if(any(is.na(basis.list)) | length(basis.list) == 0){
    record.filter <- "interactive"
  } else{
    record.filter <- "list-based"
  }

  if (record.filter == "interactive" ) {
    # interactive method for removal of basis of records
    message("Types of basis of records: ")
    print(unique(df$basisOfRecord))

    input <- readline(prompt = "Would you like to remove any types of basis of records? Enter Y for yes or N for no. ")
    new_df <- df
    while (input == "Y" | input == "y") {
      type <- readline(prompt = "Enter the type to remove exactly as it is written. ")
      new_df <- new_df[new_df$basisOfRecord != type, ]
      message("Basis of records kept: ")
      print(unique(new_df$basisOfRecord))
      input <- readline(prompt = "Would you like to remove any additional types of basis of records? Enter Y for yes or N for no. ")
    } # HELP HERE

  } else if(record.filter == "list-based"){
    message("Types of basis of records: ")
    print(unique(df$basisOfRecord))
    new_df <- data.frame()
    for (i in 1:length(basis.list)) {
      type <- basis.list[i]
      df_type <- df[agrepl(type, df$basisOfRecord, ignore.case = TRUE), ]
      new_df <- rbind(new_df, df_type)
    }
    message("Basis of records kept: ")
    print(unique(new_df$basisOfRecord))
  }

  return(new_df)
}
