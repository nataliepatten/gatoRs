#' @title fixAfterPeriod
#'
#' @description
#' This function fixes capitalization of species names when there are periods in the name.
#'
#' @details
#' Requires packages base and stringr. Does not require user input.
#'
#' @param substring is a substring from the name column of the dataframe to be fixed
#'
#' @return returns the substring with fixed capitalization
#'
#' @importFrom stringr str_to_title
#' @importFrom stringr str_length
#'

fixAfterPeriod <- function(substring) {
  index <- unlist(gregexpr(".", substring, fixed = TRUE))
  temp <- ""
  for (i in 1:length(index)) {
    if (i == length(index)) {
      temp <- paste0(temp, stringr::str_to_title(substr(substring, index[i] + 1, stringr::str_length(substring))))
    }
    else {
      temp <- paste0(temp, stringr::str_to_title(substr(substring, index[i] + 1, index[i+1])))
    }
  }
  temp2 <- stringr::str_to_title(substr(substring, 1, index))
  return(paste0(temp2,temp))
}
