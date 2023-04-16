#' @title Suppress print statements and messages
#'
#' @description
#' The `suppress_output()` function suppresses print statements and messages from a function. This
#' does not suppress warnings or errors.
#'
#' @details
#' This function requires no additional packages.
#'
#' @param function
#'
#'
#' @return output Output is the output of the passed function call.
#'
#' @keywords internal

suppress_output <- function(input) {
  return(suppressMessages(invisible(capture.output(input))))
}
