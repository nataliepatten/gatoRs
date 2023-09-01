#' @title Remove Missing Information - Prepare to merge a data frame with georeferenced and retrieved records
#'
#' @description
#' The `remove_retained()` function identifies and removes records identified with the `need_to_georeference()`
#' and `needed_record()` functions. This function should be utilized prior to merging georeferenced or retrieve records.
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
#' cleaned_data <- remove_redacted(data)
#'
#' @return A data frame with redacted records removed.
#'
#' @export

remove_retained <- function(df, info.withheld = "informationWithheld",
                            longitude = "longitude", latitude = "latitude",
                            locality = "locality", remove.type = "both"){
  if (NROW(df) == 0) return(df)


  for_georeferencing <- df
  # ID records with missing latitude and longitude
  for_georeferencing  <- for_georeferencing[is.na(for_georeferencing[[longitude]]), ]
  for_georeferencing  <- for_georeferencing[is.na(for_georeferencing[[latitude]]), ]
  # ID records with locality information included
  for_georeferencing <- for_georeferencing[!is.na(for_georeferencing[[locality]]), ]
  for_georeferencing <- for_georeferencing[!grepl("locality:  NA, occurrenceRemarks: NA, verbatimLocality: NA", for_georeferencing[[locality]], fixed = TRUE), ]


  for_withheld <- df[!is.na(df[[info.withheld]]), ]

  if(remove.type == "both"){
    remove_these <- rbind(for_georeferencing, for_withheld)
  } else if(remove.type == "georeference"){
    remove_these <- for_georeferencing
  } else if(remove.type == "withheld"){
    remove_these <-  for_withheld
  }


  cleaned <- df[!(df$ID %in% remove_these$ID),]
  return(cleaned)
}
