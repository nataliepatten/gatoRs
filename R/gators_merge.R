#' @title Merge Retained Data - Combined original dataset with georeferenced or retained records.
#'
#' @description
#' The `gators_merge()` function combines two data sets with identical columns and returns a single dataset.
#' @details
#' This function requires no additional packages.
#'
#' @param df2 A data frame downloaded with `gators_download()`.
#' @param df2 A data frame with the same columns as df1, but with observations generated through georeferencing or through data requests.
#'
#' @examples
#' cleaned_data <- gators_merge(df1, df2)
#'
#' @return A combined dataset.
#'
#' @export

gators_merge <- function(df1, df2){
  if(colnames(df1) == colnames(df2)){
  combined <- rbind(df1, df2)
  return(combined)
  } else{
    return(df1)
  }
}
