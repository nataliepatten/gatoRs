#' @title gatoRs Download - Correct classes of data frame columns
#'
#' @description
#' The `correct_class()` function corrects the classes of each column in a data frame of your queried species.
#' This function requires no additional packages.
#'
#' @details
#' "aggregator", "infraspecific.epithet", and "recorded.by" columns will be skipped if they do not exist.
#' This function requires the dplyr package.
#'
#' @param df Data frame returned by `gator_download()`.
#' @param scientific.name Default = "scientificName". The name of the [scientificName](	http://rs.tdwg.org/dwc/terms/scientificName) column in the data frame.
#' @param genus Default = "genus". The name of the [genus](http://rs.tdwg.org/dwc/terms/genus) column in the data frame.
#' @param species Default = "specificEpithet". The name of the [specificEpithet](http://rs.tdwg.org/dwc/terms/specificEpithet) column in the data frame.
#' @param infraspecific.epithet Default = "infraspecificEpithet". The name of the [infraspecificEpithet](http://rs.tdwg.org/dwc/terms/infraspecificEpithet) column in the data frame.
#' @param id Default = "ID". The name of the id column in the data frame, which contains unique IDs defined from GBIF (keys) or iDigBio (UUID).
#' @param occ.id Default = "occurrenceID". The name of the [occurrenceID](http://rs.tdwg.org/dwc/terms/occurrenceID) column in the data frame.
#' @param basis.of.record Default = "basisOfRecord". The name of the [basis of record](http://rs.tdwg.org/dwc/terms/basisOfRecord) column in the data frame.
#' @param event.date Default = "eventDate". The name of the [event date](http://rs.tdwg.org/dwc/terms/eventDate) column in the data frame.
#' @param year Default = "year". The name of the [year](http://rs.tdwg.org/dwc/terms/year) column in the data frame.
#' @param month Default = "month". The name of the [month](http://rs.tdwg.org/dwc/terms/month) column in the data frame.
#' @param day Default = "day". The name of the [day](http://rs.tdwg.org/dwc/terms/day) column in the data frame.
#' @param inst.code Default = "institutionCode". The name of the [institution code](http://rs.tdwg.org/dwc/terms/institutionCode) column in the data frame.
#' @param recorded.by Default = "recordedBy". The [recordedBy](http://rs.tdwg.org/dwc/iri/recordedBy) column in the data frame.
#' @param country Default = "country". The name of the [country](http://rs.tdwg.org/dwc/terms/country) column in the data frame.
#' @param county Default = "county". The name of the [county](http://rs.tdwg.org/dwc/terms/county) column in the data frame.
#' @param state Default = "stateProvince". The name of the [state/province](http://rs.tdwg.org/dwc/terms/stateProvince) column in the data frame.
#' @param locality Default = "locality". The name of the [locality](http://rs.tdwg.org/dwc/terms/locality) column in the data frame.
#' @param latitude Default = "latitude". The name of the [latitude](http://rs.tdwg.org/dwc/terms/decimalLatitude) column in the data frame.
#' @param longitude Default = "longitude". The name of the [longitude](http://rs.tdwg.org/dwc/terms/decimalLongitude) column in the data frame.
#' @param coord.uncertainty Default = "coordinateUncertaintyInMeters". The name of the [coordinate uncertainty](http://rs.tdwg.org/dwc/terms/coordinateUncertaintyInMeters) column in the data frame.
#' @param info.withheld Default = "informationWithheld". The name of the [information withheld](http://rs.tdwg.org/dwc/iri/informationWithheld) column in the data frame.
#' @param habitat Default = "habitat". The name of the [habitat](http://rs.tdwg.org/dwc/terms/habitat) column in the data frame.
#' @param aggregator Default = "aggregator". The name of the column in the data frame that identifies the aggregator
#' that provided the record. This is equal to iDigBio or GBIF.
#'
#' @importFrom dplyr case_when
#' @keywords internal
#' @return Returns data frame with corrected classes of each column.

correct_class <- function(df, scientific.name = "scientificName", genus = "genus",
                          species = "specificEpithet", infraspecific.epithet = "infraspecificEpithet",
                          id = "ID", occ.id = "occurrenceID",
                          basis.of.record = "basisOfRecord", event.date = "eventDate",
                          year = "year", month = "month", day = "day",
                          inst.code = "institutionCode", recorded.by = "recordedBy",
                          country = "country", county = "county", state = "stateProvince",
                          locality = "locality", latitude = "latitude",
                          longitude = "longitude",
                          coord.uncertainty = "coordinateUncertaintyInMeters",
                          info.withheld = "informationWithheld", habitat = "habitat",
                          aggregator = "aggregator"){

    df[[scientific.name]] <- dplyr::case_when(df[[scientific.name]] == "" ~ NA, .default = as.character(df[[scientific.name]]))
    df[[genus]] <- dplyr::case_when(df[[genus]] == "" ~ NA, .default = as.character(df[[genus]]))
    df[[species]] <- dplyr::case_when(df[[species]] == "" ~ NA, .default = as.character(df[[species]]))
    df[[basis.of.record]] <- dplyr::case_when(df[[basis.of.record]] == "" ~ NA, .default = as.character(df[[basis.of.record]]))
    df[[event.date]] <- dplyr::case_when(df[[event.date]] == "" ~ NA, .default = as.character(df[[event.date]]))
    df[[year]] <- dplyr::case_when(df[[year]] == "" ~ NA, .default = as.character(df[[year]]))
    df[[month]] <- dplyr::case_when(df[[month]] == "" ~ NA, .default = as.character(df[[month]]))
    df[[day]] <- dplyr::case_when(df[[day]] == "" ~ NA, .default = as.character(df[[day]]))
    df[[inst.code]] <- dplyr::case_when(df[[inst.code]] == "" ~ NA, .default = as.character(df[[inst.code]]))
    df[[country]] <- dplyr::case_when(df[[country]] == "" ~ NA, .default = as.character(df[[country]]))
    df[[county]] <- dplyr::case_when(df[[county]] == "" ~ NA, .default = as.character(df[[county]]))
    df[[state]] <- dplyr::case_when(df[[state]] == "" ~ NA, .default = as.character(df[[state]]))
    df[[locality]] <- dplyr::case_when(df[[locality]] == "" ~ NA, .default = as.character(df[[locality]]))
    df[[latitude]] <- dplyr::case_when(df[[latitude]] == "" ~ NA, .default = as.numeric(df[[latitude]]))
    df[[longitude]]<- dplyr::case_when(df[[longitude]] == "" ~ NA, .default = as.numeric(df[[longitude]]))
    df[[id]] <- dplyr::case_when(df[[id]] == "" ~ NA, .default = as.character(df[[id]]))
    df[[coord.uncertainty]] <- dplyr::case_when(df[[coord.uncertainty]] == "" ~ NA, .default = as.character(df[[coord.uncertainty]]))
    df[[info.withheld]] <- dplyr::case_when(df[[info.withheld]] == "" ~ NA, .default = as.character(df[[info.withheld]]))
    df[[habitat]] <- dplyr::case_when(df[[habitat]] == "" ~ NA, .default = as.character(df[[habitat]]))
    df[[occ.id]] <- dplyr::case_when(df[[occ.id]] == "" ~ NA, .default = as.character(df[[occ.id]]))

    if (infraspecific.epithet %in% colnames(df)){
      df[[infraspecific.epithet]] <- dplyr::case_when(df[[infraspecific.epithet]] == "" ~ NA, .default = as.character(df[[infraspecific.epithet]]))
    } else {
      df[[infraspecific.epithet]] <- NA
    }

    if (aggregator %in% colnames(df)) {
      df[[aggregator]] <- dplyr::case_when(df[[aggregator]] == "" ~ NA, .default = as.character(df[[aggregator]]))
    }

    if (recorded.by %in% colnames(df)) {
      df[[recorded.by]] <- dplyr::case_when(df[[recorded.by]] == "" ~ NA, .default = as.character(df[[recorded.by]]))
    }

    return(df)
}
