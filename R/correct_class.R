#' @title gatoRs Download - Correct classes of data frame columns
#'
#' @description
#' The `correct_class()` function corrects the classes of each column in a data frame of your queried species.
#' This function requires no additional packages.
#'
#' @param df Data frame returned by `gator_download()`.
#' @param scientific.name Default = "scientificName". The name of the scientific name column in the data frame.
#' @param genus Default = "genus". The name of the genus column in the data frame.
#' @param species Default = "specificEpithet". The name of the specific epithet column in the data frame.
#' @param basis.of.record Default = "basisOfRecord". The name of the basis of record column in the data frame.
#' @param event.date Default = "eventDate". The name of the event date column in the data frame.
#' @param inst.code Default = "institutionCode". The name of the institution code column in the data frame.
#' @param col.code Default = "collectionCode". The name of the collection code column in the data frame.
#' @param country Default = "country". The name of the country column in the data frame.
#' @param county Default = "county". The name of the county column in the data frame.
#' @param state Default = "stateProvince". The name of the state/province column in the data frame.
#' @param locality Default = "locality". The name of the locality column in the data frame.
#' @param latitude Default = "latitude". The name of the latitude column in the data frame.
#' @param longitude Default = "longitude". The name of the longitude column in the data frame.
#' @param id Default = "identificationID". The name of the identification id column in the data frame.
#' @param coord.uncertainty Default = "coordinateUncertaintyInMeters". The name of the coordinate uncertainty column in the data frame.
#' @param info.withheld Default = "informationWithheld". The name of the information withheld column in the data frame.
#' @param habitat Default = "habitat". The name of the habitat column in the data frame.
#' @param infraspecific.epithet Default = "infraspecificEpithet". The name of the infraspecific epithet column in the data frame.
#' @param col.id Default = "collectionID". The name of the collection id column in the data frame.
#'
#' @keywords internal
#' @return Returns data frame with corrected classes of each column.

correct_class <- function(df, scientific.name = "scientificName", genus = "genus",
                          species = "specificEpithet", basis.of.record = "basisOfRecord",
                          event.date = "eventDate", inst.code = "institutionCode",
                          col.code = "collectionCode", country = "country",
                          county = "county", state = "stateProvince",
                          locality = "locality", latitude = "latitude",
                          longitude = "longitude", id = "identificationID",
                          coord.uncertainty = "coordinateUncertaintyInMeters",
                          info.withheld = "informationWithheld", habitat = "habitat",
                          infraspecific.epithet = "infraspecificEpithet", col.id = "collectionID"){

    df[[scientific.name]] <- as.character(df[[scientific.name]])
    df[[genus]] <- as.character(df[[genus]])
    df[[species]] <- as.character(df[[species]])
    df[[basis.of.record]] <- as.character(df[[basis.of.record]])
    df[[event.date]] <- as.character(df[[event.date]])
    df[[inst.code]] <- as.character(df[[inst.code]])
    df[[col.code]] <- as.character(df[[col.code]])
    df[[country]] <- as.character(df[[country]])
    df[[county]] <- as.character(df[[county]])
    df[[state]] <- as.character(df[[state]])
    df[[locality]] <- as.character(df[[locality]])
    df[[latitude]] <- as.numeric(df[[latitude]])
    df[[longitude]]<- as.numeric(df[[longitude]])
    df[[id]] <- as.character(df[[id]])
    df[[coord.uncertainty]] <- as.numeric(df[[coord.uncertainty]])
    df[[info.withheld]] <- as.character(df[[info.withheld]])
    df[[habitat]] <- as.character(df[[habitat]])

    if (infraspecific.epithet %in% colnames(df)){
      df[[infraspecific.epithet]] <- as.character(df[[infraspecific.epithet]])
    } else {
      df[[infraspecific.epithet]] <- NA
    }

    if (col.id %in% colnames(df)){
      df[[col.id]] <- as.character(df[[col.id]])
    } else {
      df[[col.id]] <- NA
    }
    return(df)
}
