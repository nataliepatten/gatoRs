#' @title Used in gators_download() - Fix taxonomic name capitalization
#'
#' @description
#' The `fix_names()` function fixes the capitalization of species names in the data frame provided to align
#' with accepted capitalization standards.
#'
#' @details
#' This function uses the `fixAfterPeriod()` function. This function requires package stringr.
#'
#' @param df Data frame with name column to be fixed.
#' @inheritParams correct_class
#'
#' @examples
#' data <- fix_names(data)
#'
#' @return Returns df with fixed capitalization in name column.
#'
#' @importFrom stringr str_to_sentence str_length str_to_lower str_to_title
#' @export

fix_names <- function(df, scientific.name = "scientificName") {
  # if the data frame is empty
  if (NROW(df) == 0) return(df)

  for (i in 1:NROW(df)) {
    if (length(unlist(gregexpr(" ", df[[scientific.name]][i]))) > 1) {
      if (grepl("var.", df[[scientific.name]][i], ignore.case = TRUE)) {
        index1 <- unlist(gregexpr(" ", df[[scientific.name]][i]))[2]
        genus_species <- stringr::str_to_sentence(substr(df[[scientific.name]][i], 1, index1))
        if (length(unlist(gregexpr(" ", df[[scientific.name]][i]))) < 4) {
          index2 <- stringr::str_length(df[[scientific.name]][i])
        }
        else {
          index2 <- unlist(gregexpr(" ", df[[scientific.name]][i]))[4]
        }
        varString <- stringr::str_to_lower(substr(df[[scientific.name]][i], index1 + 1, index2))
        afterVar <- stringr::str_to_title(substr(df[[scientific.name]][i], index2 + 1, stringr::str_length(df[[scientific.name]][i])))
        if (length(unlist(gregexpr(".", afterVar, fixed = TRUE))) > 1) {
          afterVar = fixAfterPeriod(afterVar)
        }
        df[[scientific.name]][i] <- paste0(genus_species, varString, afterVar)
      }
      else if (grepl("var ", df[[scientific.name]][i], ignore.case = TRUE)) {
        index1 <- unlist(gregexpr(" ", df[[scientific.name]][i]))[2]
        genus_species <- stringr::str_to_sentence(substr(df[[scientific.name]][i], 1, index1))
        index2 <- unlist(gregexpr(" ", df[[scientific.name]][i]))[3]
        if (length(unlist(gregexpr(" ", df[[scientific.name]][i]))) < 4) {
          index3 <- stringr::str_length(df[[scientific.name]][i])
        }
        else {
          index3 <- unlist(gregexpr(" ", df[[scientific.name]][i]))[4]
        }
        varString <- paste0(substr(df[[scientific.name]][i], index1 + 1, index2 - 1), ".")
        varString <- paste0(varString, substr(df[[scientific.name]][i], index2, index3))
        if (index3 != stringr::str_length(df[[scientific.name]][i])) {
          afterVar <- stringr::str_to_title(substr(df[[scientific.name]][i], index3 + 1, stringr::str_length(df[[scientific.name]][i])))
        }
        else {
          afterVar <- ""
        }
        if (length(unlist(gregexpr(".", afterVar, fixed = TRUE))) > 1) {
          afterVar = fixAfterPeriod(afterVar)
        }

        df[[scientific.name]][i] <- paste0(genus_species, varString, afterVar)
      }
      else if (grepl("subsp.", df[[scientific.name]][i], ignore.case = TRUE)) {
        index1 <- unlist(gregexpr(" ", df[[scientific.name]][i]))[2]
        genus_species <- stringr::str_to_sentence(substr(df[[scientific.name]][i], 1, index1))
        if (length(unlist(gregexpr(" ", df[[scientific.name]][i]))) < 4) {
          index2 <- stringr::str_length(df[[scientific.name]][i])
        }
        else {
          index2 <- unlist(gregexpr(" ", df[[scientific.name]][i]))[4]
        }
        subspString <- substr(df[[scientific.name]][i], index1 +1, index2)
        afterSubsp <- stringr::str_to_title(substr(df[[scientific.name]][i], index2 + 1, stringr::str_length(df[[scientific.name]][i])))
        if (length(unlist(gregexpr(".", afterSubsp, fixed = TRUE))) > 1) {
          afterSubsp <- fixAfterPeriod(afterSubsp)
        }

        df[[scientific.name]][i] <- paste0(genus_species, subspString, afterSubsp)
      }
      else if (grepl("subsp ", df[[scientific.name]][i], ignore.case = TRUE)) {
        index1 <- unlist(gregexpr(" ", df[[scientific.name]][i]))[2]
        genus_species <- stringr::str_to_sentence(substr(df[[scientific.name]][i], 1, index1))

        index2 <- unlist(gregexpr(" ", df[[scientific.name]][i]))[3]
        index3 <- unlist(gregexpr(" ", df[[scientific.name]][i]))[4]

        subspString <- paste0(substr(df[[scientific.name]][i], index1 + 1, index2 - 1), ".")
        subspString <- paste0(subspString, substr(df[[scientific.name]][i], index2, index3))
        afterSubsp <- stringr::str_to_title(substr(df[[scientific.name]][i], index3 + 1, stringr::str_length(df[[scientific.name]][i])))
        if (length(unlist(gregexpr(".", afterSubsp, fixed = TRUE))) > 1) {
          afterSubsp <-  fixAfterPeriod(afterSubsp)
        }

        df[[scientific.name]][i] <- paste0(genus_species, subspString, afterSubsp)
      }
      else {
        index <- unlist(gregexpr(" ", df[[scientific.name]][i]))[2]
        genus_species <- stringr::str_to_sentence(substr(df[[scientific.name]][i], 1, index))

        authorityString <- stringr::str_to_title(substr(df[[scientific.name]][i], index + 1, stringr::str_length(df[[scientific.name]][i])))
        if (length(unlist(gregexpr(".", authorityString, fixed = TRUE))) > 0) {
          authorityString <- fixAfterPeriod(authorityString)
        }

        df[[scientific.name]][i] <- paste0(genus_species, authorityString)
      }
    }
    else {
      df[[scientific.name]][i] <- stringr::str_to_sentence(df[[scientific.name]][i])
    }

    if (grepl(" Ex ", df[[scientific.name]][i], ignore.case = FALSE)) {
      index <- unlist(gregexpr(" Ex ", df[[scientific.name]][i]))

      before <- substr(df[[scientific.name]][i], 1, index - 1)
      exString <- stringr::str_to_lower(substr(df[[scientific.name]][i], index, index + 2))
      after <- substr(df[[scientific.name]][i], index + 3, stringr::str_length(df[[scientific.name]][i]))
      df[[scientific.name]][i] <- paste0(before, exString, after)
    }
  }
  return(df)
}




