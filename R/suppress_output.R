#' @title Suppress print statements and messages
#'
#' @description
#' The `suppress_output()` function suppresses print statements and messages from a function. This
#' does not suppress warnings or errors.
#'
#' @details
#' This function requires no additional packages.
#'
#' @param input
#'
#' @importFrom utils capture.output
#' @return output Output is the output of the passed function call.
#'
#' @keywords internal

suppress_output <- function(input) {
  # https://stackoverflow.com/questions/48499400/suppress-automatic-output-to-console-in-r
  return(suppressMessages(invisible(utils::capture.output(input))))
}
