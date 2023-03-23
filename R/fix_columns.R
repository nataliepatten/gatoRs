#' @title Used in gators_download() - Fill out taxonomic name columns
#'
#' @description
#' The `fix_columns()` function fills out the taxonomic name columns based on available information in the data set.
#' For example, if a row has a name provided for the scientificName column, this information will be used
#' to generate the respective genus, specificEpithet, and infraspecificEpithet columns for that row.
#'
#' @details
#' This function requires package stringr.
#'
#' @param df Data frame of occurrence records.
#' @inheritParams correct_class
#'
#' @examples
#' data <- fix_columns(data)
#'
#' @return Returns the original data frame with the specified columns.
#'
#' @importFrom stringr str_length
#' @export

fix_columns <- function(df, scientific.name = "scientificName", genus = "genus",
                        species = "specificEpithet", infraspecific.epithet = "infraspecificEpithet") {
  # if the data frame is empty
  if (NROW(df) == 0) return(df)

  for (i in 1: nrow(df)) {
    if (is.na(df[[scientific.name]][i])) {
      if (!is.na(df[[infraspecific.epithet]][i]) & !is.na(df[[genus]][i]) & !is.na(df[[species]][i])) {
        df[[scientific.name]][i] <- paste0(df[[genus]][i], " ", df[[species]][i], " ",
                                      df[[infraspecific.epithet]][i])
      }
      else if (!is.na(df[[genus]][i]) & !is.na(df[[species]][i])) {
        df[[scientific.name]][i] <- paste0(df[[genus]][i], " ", df[[species]][i])
      }
    }
    else {
      df[[scientific.name]][i] <- df[[scientific.name]][i]
    }


    index1 <- unlist(gregexpr(" ", df[[scientific.name]][i]))[1]
    index2 <- NULL
    if (length(unlist(gregexpr(" ", df[[scientific.name]][i]))) > 1)
      index2 <- unlist(gregexpr(" ", df[[scientific.name]][i]))[2]

    if (is.na(df[[genus]][i])) {
      df[[genus]][i] <- substr(df[[scientific.name]][i], 1, index1 - 1)
    if (is.na(df[[species]][i])) {
      if (!is.null(index2))
        df[[species]][i] <- substr(df[[scientific.name]][i], index1 + 1, index2 - 1)
      else
        df[[species]][i] <- substr(df[[scientific.name]][i], index1 + 1, stringr::str_length(df[[scientific.name]][i]))
    }
    if (is.na(df[[infraspecific.epithet]][i]) & !is.null(index2))
      df[[infraspecific.epithet]][i] <- substr(df[[scientific.name]][i], index2 + 1,
                                           stringr::str_length(df[[scientific.name]][i]))
    }
  }
  return(df)
}
