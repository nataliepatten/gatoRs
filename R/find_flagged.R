#' @title find_flagged
#'
#' @description
#' This function allows you to find and map possible problematic points and remove these points, if desired.
#'
#' @details
#' This function requires packages dplyr, CoordinateCleaner, base, leaflet, magrittr. This function requires user input.
#'
#' @param df is a dataframe of occurrence records.
#'
#' @return Return a copy of the original dataframe provided as input but without any points the user chose to remove.
#'
#' @export

find_flagged <- function(df) {
  # filter for points that have lat/long within a range and that are not 0
  df <- df %>%
        dplyr::filter(!is.na(Longitude)) %>%
        dplyr::filter(!is.na(Latitude))
  df <- df %>%
        dplyr::filter(Longitude < 180) %>%
        dplyr::filter(Latitude < 180)
  df <- df %>%
        dplyr::filter(Longitude > -180) %>%
        dplyr::filter(Latitude > -180)
  df <- df %>%
        dplyr::filter(Longitude != 0) %>%
        dplyr::filter(Latitude != 0)
  # round for precision
  df$Latitude <- round(df$Latitude, digits = 2)
  df$Longitude <- round(df$Longitude, digits = 2)
  df2 <- CoordinateCleaner::clean_coordinates(df, lon = "Longitude", lat = "Latitude", species = "name", value = "spatialvalid")
  # find the flagged points
  flagged <- df2[df2$.summary == "FALSE", ]
  flagged$index <- as.character(1:nrow(flagged))
  # make new column with (latitude, longitude) and point number if there are any flagged points
  if (nrow(flagged) > 0) {
    flagged$coordinates <- paste0("(", flagged$Latitude, ", ", flagged$Longitude, ")", ", point #", flagged$index)
  }
  else {
    print("No flagged points found.")
    return(df)
  }
  # make a map of the flagged points; hovering over points will show lat/long, clicking on points will show species name
  coordinates <- flagged$coordinates
  icons <- awesomeIcons(icon = "fa-leaf", iconColor = "#3CB371", library = "fa", squareMarker = TRUE, markerColor = "lightgreen")
  map <- leaflet(data = flagged) %>%
    leaflet::addProviderTiles(providers$OpenStreetMap) %>%
    leaflet::addAwesomeMarkers(lng = ~Longitude, lat = ~Latitude, icon = icons, popup = ~coordinates, label = ~name, layerId = ~index) %>%
    leaflet::addMiniMap(toggleDisplay = TRUE) %>%
  print(map)

  # prompt the user to choose to zoom in or not
  input <- readline(prompt = "Would you like to zoom in to a particular region? Enter Y for yes or N for no. ")
  if (input == "Y" | input == 'y') {
    # find the region to display
    lowlong <- readline(prompt = "Please enter the lower bound for longitude: ")
    lowlong <- as.numeric(lowlong)
    highlong <- readline(prompt = "Please enter the upper bound for longitude: ")
    highlong <- as.numeric(highlong)
    lowlat <- readline(prompt = "Please enter the lower bound for latitude: ")
    lowlat <- as.numeric(lowlat)
    highlat <- readline(prompt = "Please enter the upper bound for latitude: ")
    highlat <- as.numeric(highlat)
    # print the map of the world with the flagged points in the region
    map <- map %>%
      fitBounds(lowlong, lowlat, highlong, highlat)
    print(map)
  }
  if (input == "N" | input == 'n') {
    print("To zoom in and out, you can click on the plus and minus icons, respectively.")
  }

  newdf <- df

  # prompt the user to choose whether to remove any points
  input <- readline(prompt = "Would you like to remove a rectangular region of points from the dataframe? Enter Y for yes or N for no. ")
  if (input == "Y" | input == 'y') {
    # find the region of points to remove
    print("To remove points, you will be prompted to enter the region from which to remove points from the dataframe. You may repeat this process as many times as you would like.")
    lowlong <- readline(prompt = "Please enter the lower bound for longitude: ")
    lowlong <- as.numeric(lowlong)
    highlong <- readline(prompt = "Please enter the upper bound for longitude: ")
    highlong <- as.numeric(highlong)
    lowlat <- readline(prompt = "Please enter the lower bound for latitude: ")
    lowlat <- as.numeric(lowlat)
    highlat <- readline(prompt = "Please enter the upper bound for latitude: ")
    highlat <- as.numeric(highlat)
    # find the flagged points in the region entered
    flaggedFiltered <- flagged %>%
      dplyr::filter(Longitude >= lowlong & Longitude <= highlong & Latitude >= lowlat & Latitude <= highlat)
    for (i in 1:nrow(flaggedFiltered)) {
      for (j in 1: nrow(flagged)) {
        if (flaggedFiltered$ID[i] == flagged$ID[j]) {
          # remove the "removed" points from the map
          map <- map %>%
            removeMarker(flagged$index[j])
          break
        }
      }
    }
    print(map)
    # make a copy of the original dataframe, removing any points with the same lat/long as the flagged points in the region
    for(i in 1:NROW(flaggedFiltered)) {
      newdf <- newdf %>%
        dplyr::filter(Longitude != flaggedFiltered$Longitude[i] & Latitude != flaggedFiltered$Latitude[i])
    }

    # find the number of points removed from the original dataframe, and display it
    pointsRemoved = NROW(df) - NROW(newdf)
    print(paste0(pointsRemoved, " points removed."))


    # prompt user to choose whether to keep removing points
    while(readline(prompt = "Would you like to select another region from which to remove points from the dataframe? Enter Y for yes or N for no. ") == "Y") {
      # find the region of points to remove
      lowlong <- readline(prompt = "Please enter the lower bound for longitude: ")
      lowlong <- as.numeric(lowlong)
      highlong <- readline(prompt = "Please enter the upper bound for longitude: ")
      highlong <- as.numeric(highlong)
      lowlat <- readline(prompt = "Please enter the lower bound for latitude: ")
      lowlat <- as.numeric(lowlat)
      highlat <- readline(prompt = "Please enter the upper bound for latitude: ")
      highlat <- as.numeric(highlat)
      # create a variable for the current number of points in the dataframe
      current = NROW(newdf)
      # find the flagged points in the region entered
      flaggedFiltered <- flagged %>%
        dplyr::filter(Longitude >= lowlong & Longitude <= highlong & Latitude >= lowlat & Latitude <= highlat)
      for (i in 1:nrow(flaggedFiltered)) {
        for (j in 1: nrow(flagged)) {
          if (flaggedFiltered$ID[i] == flagged$ID[j]) {
            # remove the "removed" points from the map
            map <- map %>%
              removeMarker(flagged$index[j])
            break
          }
        }
      }
      print(map)
      # remove any points with the same lat/long as the flagged points in the region
      for(i in 1:NROW(flaggedFiltered)) {
        newdf <- newdf %>%
          dplyr::filter(Longitude != flaggedFiltered$Longitude[i] & Latitude != flaggedFiltered$Latitude[i])
      }
      # find the number of points removed and display it
      pointsRemoved = current - NROW(newdf)
      print(paste0(pointsRemoved, " points removed."))
    }
  }
  input <- readline(prompt = "Would you like to remove any individual points? Enter Y for yes or N for no. ")
  while (input == "Y" | input == 'y') {
    point_num <- readline(prompt = "Enter the point number: ")
    map <- map %>%
      removeMarker(point_num)
    print(map)
    newdf <- newdf %>%
      dplyr::filter(Longitude != flagged$Longitude[as.integer(point_num)] & Latitude != flagged$Latitude[as.integer(point_num)])
    input <- readline(prompt = "Would you like to remove additional individual points? Enter Y for yes or N for no. ")
  }
  # return the new dataframe with the removed points
  return(newdf)
}
