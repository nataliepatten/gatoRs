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
#'
#' @examples
#' data <- fix_columns(data)
#'
#' @return Returns the original data frame with the specified columns.
#'
#' @importFrom stringr str_length
#' @export

fix_columns <- function(df) {
  # if the data frame is empty
  if (NROW(df) == 0) return(df)

  for (i in 1: nrow(df)) {
    if (is.na(df$scientificName[i])) {
      if (!is.na(df$infraspecificEpithet[i]) & !is.na(df$genus[i]) & !is.na(df$specificEpithet[i])) {
        df$scientificName[i] <- paste0(df$genus[i], " ", df$specificEpithet[i], " ",
                                      df$infraspecificEpithet[i])
      }
      else if (!is.na(df$genus[i]) & !is.na(df$specificEpithet[i])) {
        df$scientificName[i] <- paste0(df$genus[i], " ", df$specificEpithet[i])
      }
    }
    else {
      df$scientificName[i] <- df$scientificName[i]
    }


    index1 <- unlist(gregexpr(" ", df$scientificName[i]))[1]
    index2 <- NULL
    if (length(unlist(gregexpr(" ", df$scientificName[i]))) > 1)
      index2 <- unlist(gregexpr(" ", df$scientificName[i]))[2]

    if (is.na(df$genus[i])) {
      df$genus[i] <- substr(df$scientificName[i], 1, index1 - 1)
    if (is.na(df$specificEpithet[i])) {
      if (!is.null(index2))
        df$specificEpithet[i] <- substr(df$scientificName[i], index1 + 1, index2 - 1)
      else
        df$specificEpithet[i] <- substr(df$scientificName[i], index1 + 1, stringr::str_length(df$scientificName[i]))
    }
    if (is.na(df$infraspecificEpithet[i]) & !is.null(index2))
      df$infraspecificEpithet[i] <- substr(df$scientificName[i], index2 + 1,
                                           stringr::str_length(df$scientificName[i]))
    }
  }
  return(df)
}
