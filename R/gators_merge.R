#' @title Merge Retained Data - Combined original data set with georeferenced or retained records.
#'
#' @description
#' The `gators_merge()` function combines two data sets with identical column names and returns a single data set.
#'
#' @details
#' This function requires no additional packages.
#'
#' @param df1 A data frame downloaded with `gators_download()` and prepared using `remove_missing`.
#' @param df2 A data frame with the same columns as df1, but with observations generated through georeferencing or through data requests.
#'
#' @examples
#' removed_missing <- remove_missing(data)
#' needs_geo <- need_to_georeference(data)
#' # fill in manually georeferenced data into needs_geo...
#' merged_data <- gators_merge(removed_missing, needs_geo)
#' needs_data <- needed_records(data)
#' # fill in missing information with a data request...
#' merged_data <- gators_merge(merged_data, needs_data)
#'
#' @return A combined data set.
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
