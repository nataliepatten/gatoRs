#' @title fix_columns
#'
#' @description
#' This function fills out the taxonomic name columns based on available information.
#'
#' @details
#' This function requires packages dplyr, base, magrittr, stringr.
#' This function uses the correct_class function.
#'
#' @param df is a data frame of occurrence records
#'
#' @return Returns the original data frame with the specified columns
#'
#' @importFrom stringr str_length

fix_columns <- function(df) {
  for (i in 1: nrow(df)) {
    if (is.na(df$scientificName[i])) {
      if (!is.na(df$infraspecificEpithet[i]) & !is.na(df$genus[i]) & !is.na(df$species[i])) {
        df$scientificName[i] <- paste0(df$genus[i], " ", df$species[i], " ",
                                      df$infraspecificEpithet[i])
      }
      else if (!is.na(df$genus[i]) & !is.na(df$species[i])) {
        df$scientificName[i] <- paste0(df$genus[i], " ", df$species[i])
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
    if (is.na(df$species[i])) {
      if (!is.null(index2))
        df$species[i] <- substr(df$scientificName[i], index1 + 1, index2 - 1)
      else
        df$species[i] <- substr(df$scientificName[i], index1 + 1, str_length(df$scientificName[i]))
    }
    if (is.na(df$infraspecificEpithet[i]) & !is.null(index2))
      df$infraspecificEpithet[i] <- substr(df$scientificName[i], index2 + 1,
                                           str_length(df$scientificName[i]))
    }
  }
  return(df)
}
