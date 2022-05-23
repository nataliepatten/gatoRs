#' correct_class
#'
#' This function corrects the classes of each column in a data frame of your queried species.
#' This function requires the package base.
#' @param df is a data frame.
#' @return a data frame with corrected classes of each column.

correct_class <- function(df){
  df$scientificName <- as.character(df$scientificName)
  df$genus <- as.character(df$genus)
  df$specificEpithet <- as.character(df$specificEpithet)

  if ("infraspecificEpithet" %in% colnames(df))
    df$infraspecificEpithet <- as.character(df$infraspecificEpithet)

  df$basisOfRecord <- as.character(df$basisOfRecord)
  df$eventDate <- as.character(df$eventDate)
  df$institutionCode <- as.character(df$institutionCode)
  df$collectionCode <- as.character(df$collectionCode)

  if ("collectionID" %in% colnames(df))
    df$collectionID <- as.character(df$collectionID)

  df$country <- as.character(df$country)
  df$county <- as.character(df$county)
  df$stateProvince <- as.character(df$stateProvince)
  df$locality <- as.character(df$locality)
  df$latitude <- as.numeric(df$latitude)
  df$longitude<- as.numeric(df$longitude)
  df$identificationID <- as.character(df$identificationID)
  df$coordinateUncertaintyInMeters <- as.character(df$coordinateUncertaintyInMeters)
  df$informationWithheld <- as.character(df$informationWithheld)
  df$habitat <- as.character(df$habitat)
  return(df)
}
