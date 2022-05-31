#' @title fix_names
#'
#' @description
#' This function fixes the capitalization of species names.
#'
#' @details
#' This function requires packages base and stringr. This function does not require user input.
#'
#' @param df is a dataframe with name column to be fixed
#'
#' @return returns df with fixed capitalization in name column
#'
#' @importFrom stringr str_to_sentence
#' @importFrom stringr str_length
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_to_title
#'

fix_names <- function(df) {
  for (i in 1:NROW(df)) {
    if (length(unlist(gregexpr(" ", df$scientificName[i]))) > 1) {
      if (grepl("var.", df$scientificName[i], ignore.case = TRUE)) {
        index1 <- unlist(gregexpr(" ", df$scientificName[i]))[2]
        genus_species <- stringr::str_to_sentence(substr(df$scientificName[i], 1, index1))
        if (length(unlist(gregexpr(" ", df$scientificName[i]))) < 4) {
          index2 <- stringr::str_length(df$scientificName[i])
        }
        else {
          index2 <- unlist(gregexpr(" ", df$scientificName[i]))[4]
        }
        varString <- stringr::str_to_lower(substr(df$scientificName[i], index1 + 1, index2))
        afterVar <- stringr::str_to_title(substr(df$scientificName[i], index2 + 1, stringr::str_length(df$scientificName[i])))
        if (length(unlist(gregexpr(".", afterVar, fixed = TRUE))) > 1) {
          afterVar = fixAfterPeriod(afterVar)
        }
        df$scientificName[i] <- paste0(genus_species, varString, afterVar)
      }
      else if (grepl("var ", df$scientificName[i], ignore.case = TRUE)) {
        index1 <- unlist(gregexpr(" ", df$scientificName[i]))[2]
        genus_species <- stringr::str_to_sentence(substr(df$scientificName[i], 1, index1))
        index2 <- unlist(gregexpr(" ", df$scientificName[i]))[3]
        if (length(unlist(gregexpr(" ", df$scientificName[i]))) < 4) {
          index3 <- stringr::str_length(df$scientificName[i])
        }
        else {
          index3 <- unlist(gregexpr(" ", df$scientificName[i]))[4]
        }
        varString <- paste0(substr(df$scientificName[i], index1 + 1, index2 - 1), ".")
        varString <- paste0(varString, substr(df$scientificName[i], index2, index3))
        if (index3 != stringr::str_length(df$scientificName[i])) {
          afterVar <- stringr::str_to_title(substr(df$scientificName[i], index3 + 1, stringr::str_length(df$scientificName[i])))
        }
        else {
          afterVar <- ""
        }
        if (length(unlist(gregexpr(".", afterVar, fixed = TRUE))) > 1) {
          afterVar = fixAfterPeriod(afterVar)
        }

        df$scientificName[i] <- paste0(genus_species, varString, afterVar)
      }
      else if (grepl("subsp.", df$scientificName[i], ignore.case = TRUE)) {
        index1 <- unlist(gregexpr(" ", df$scientificName[i]))[2]
        genus_species <- stringr::str_to_sentence(substr(df$scientificName[i], 1, index1))
        if (length(unlist(gregexpr(" ", df$scientificName[i]))) < 4) {
          index2 <- stringr::str_length(df$scientificName[i])
        }
        else {
          index2 <- unlist(gregexpr(" ", df$scientificName[i]))[4]
        }
        subspString <- substr(df$scientificName[i], index1 +1, index2)
        afterSubsp <- stringr::str_to_title(substr(df$scientificName[i], index2 + 1, stringr::str_length(df$scientificName[i])))
        if (length(unlist(gregexpr(".", afterSubsp, fixed = TRUE))) > 1) {
          afterSubsp <- fixAfterPeriod(afterSubsp)
        }

        df$scientificName[i] <- paste0(genus_species, subspString, afterSubsp)
      }
      else if (grepl("subsp ", df$scientificName[i], ignore.case = TRUE)) {
        index1 <- unlist(gregexpr(" ", df$scientificNames[i]))[2]
        genus_species <- stringr::str_to_sentence(substr(df$scientificName[i], 1, index1))

        index2 <- unlist(gregexpr(" ", df$scientificName[i]))[3]
        index3 <- unlist(gregexpr(" ", df$scientificName[i]))[4]

        subspString <- paste0(substr(df$scientificName[i], index1 + 1, index2 - 1), ".")
        subspString <- paste0(subspString, substr(df$scientificName[i], index2, index3))
        afterSubsp <- stringr::str_to_title(substr(df$scientificName[i], index3 + 1, stringr::str_length(df$scientificName[i])))
        if (length(unlist(gregexpr(".", afterSubsp, fixed = TRUE))) > 1) {
          afterSubsp <-  fixAfterPeriod(afterSubsp)
        }

        df$scientificName[i] <- paste0(genus_species, subspString, afterSubsp)
      }
      else {
        index <- unlist(gregexpr(" ", df$scientificName[i]))[2]
        genus_species <- stringr::str_to_sentence(substr(df$scientificName[i], 1, index))

        authorityString <- stringr::str_to_title(substr(df$scientificName[i], index + 1, stringr::str_length(df$scientificName[i])))
        if (length(unlist(gregexpr(".", authorityString, fixed = TRUE))) > 0) {
          authorityString <- fixAfterPeriod(authorityString)
        }

        df$scientificName[i] <- paste0(genus_species, authorityString)
      }
    }
    else {
      df$scientificName[i] <- stringr::str_to_sentence(df$scientificName[i])
    }
    if (grepl("ex ", df$scientificName[i], ignore.case = TRUE)) {
      index1 <- unlist(gregexpr("ex ", df$scientificName[i]))
      index2 <- unlist(gregexpr("Ex", df$scientificName[i]))
      if (index1 == -1) {
        index = index2
      }
      else {
        index = index1
      }
      before <- substr(df$scientificName[i], 1, index - 1)
      exString <- stringr::str_to_lower(substr(df$scientificName[i], index, index + 1))
      after <- substr(df$scientificName[i], index + 2, stringr::str_length(df$scientificName[i]))
      df$scientificName[i] <- paste0(before, exString, after)
    }
  }
  return(df)
}




