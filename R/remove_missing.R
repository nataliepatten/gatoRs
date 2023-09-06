#' @title Remove Missing Information - Prepare to merge a data frame with georeferenced and retrieved records
#'
#' @description
#' The `remove_missing()` function identifies and removes records identified with the `need_to_georeference()`
#' and `needed_records()` functions. This function should be utilized prior to merging georeferenced or retrieved records.
#'
#' @details
#' This function requires no additional packages.
#'
#' @param df A data frame downloaded with `gators_download()`.
#' @param remove.type Default equal to "both" indicating records identified with the `need_to_georeference()` function
#'    and `needed_records()` function are removed from the data frame. If equal to "georeference" then only records
#'    identified by the `need_to_georeference()` function are removed. If equal to "withheld" then only records identified
#'    with the `needed_records()` function are removed.
#' @inheritParams correct_class
#'
#' @examples
#' cleaned_data <- remove_missing(data)
#'
#' @return A data frame with records containing missing information removed.
#' Information about the columns in the returned data frame can be found in the documentation for `gators_download()`.
#'
#' @export

remove_missing <- function(df, remove.type = "both", info.withheld = "informationWithheld",
                            longitude = "longitude", latitude = "latitude",
                            locality = "locality", id = "ID"){
  if (NROW(df) == 0) return(df)


  for_georeferencing <- need_to_georeference(df)
  for_withheld <- needed_records(df)

  if(remove.type == "both"){
    remove_these <- rbind(for_georeferencing, for_withheld)
  } else if(remove.type == "georeference"){
    remove_these <- for_georeferencing
  } else if(remove.type == "withheld"){
    remove_these <-  for_withheld
  }


  cleaned <- df[!df[[id]] %in% remove_these[[id]],]
  return(cleaned)
}
