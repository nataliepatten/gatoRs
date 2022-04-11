#' fix_columns
#'
#' This function selects columns to keep and corrects the class of each.
#' This function requires packages dplyr, base, lubridate, magrittr, stringr.
#' This function uses the correct_class function.
#' @param df is a dataframe of occurrence records
#' @return df is the original dataframe with the specified columns

fix_columns <- function(df) {
  for (i in 1: nrow(df)) {
    if (is.na(df$scientificName[i])) {
      if (!is.na(df$infraspecificEpithet[i]) & !is.na(df$genus[i]) & !is.na(df$species[i])) {
        df$name[i] <- paste0(df$genus[i], " ", df$species[i], " ",
                                      df$infraspecificEpithet[i])
      }
      else if (!is.na(df$genus[i]) & !is.na(df$species[i])) {
        df$name[i] <- paste0(df$genus[i], " ", df$species[i])
      }
    }
    else {
      df$name[i] <- df$scientificName[i]
    }


    index1 <- unlist(gregexpr(" ", df$name[i]))[1]
    index2 <- NULL
    if (length(unlist(gregexpr(" ", df$name[i]))) > 1)
      index2 <- unlist(gregexpr(" ", df$name[i]))[2]

    if (is.na(df$genus[i])) {
      df$genus[i] <- substr(df$name[i], 1, index1 - 1)
    if (is.na(df$species[i])) {
      if (!is.null(index2))
        df$species[i] <- substr(df$name[i], index1 + 1, index2 - 1)
      else
        df$species[i] <- substr(df$name[i], index1 + 1, str_length(df$name[i]))
    }
    if (is.na(df$infraspecificEpithet[i]) & !is.null(index2))
      df$infraspecificEpithet[i] <- substr(df$name[i], index2 + 1,
                                           str_length(df$name[i]))
    }
  }

  print("Types of basis of records: ")
  print(unique(df$basisOfRecord))

  input <- readline(prompt = "Would you like to remove any types of basis of records? Enter Y for yes or N for no. ")

  while (input == "Y" | input == "y") {
    type <- readline(prompt = "Enter the type to remove exactly as it is written. ")
    df <- df %>%
      dplyr::filter(basisOfRecord != type)
    input <- readline(prompt = "Would you like to remove any additional types of basis of records? Enter Y for yes or N for no. ")
  }
  return(df)
}
