#' @title Check columns of data frame
#'
#' @description
#' The `check_columns()` function provides the columns requested.
#' This function requires no additional packages.
#'
#' @param spocc_name A list of columns from the query.
#' @param fields A list of fields.
#'
#' @keywords internal
#' @return Returns a data frame with desired columns.



check_columns <- function(spocc_name, fields){
  diff1 <- setdiff(fields, colnames(spocc_name))
  newframe <- data.frame(matrix(, nrow = 1 , ncol= as.numeric(length(diff1))))
  colnames(newframe) <- diff1
  spocc_name_new <- cbind(spocc_name, newframe)
  return(spocc_name_new)
}
