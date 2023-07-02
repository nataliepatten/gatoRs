#' @title Locality Cleaning - Find possibly problematic occurrence records
#'
#' @description
#' The `process_flagged()` function allows you to find and map possible problematic points and
#' manually inspect and remove these points, if desired. When running the function interactively you can
#' hover over a point to see the record's scientific name, and click on a point to see the record's coordinates.
#'
#'
#' @details
#' This function requires packages CoordinateCleaner, leaflet, and magrittr.
#' This function requires interactive user input.
#'
#' @param df Data frame of occurrence records returned from `gators_download()`.
#' @param interactive Default = TRUE. The interactive option allows for a visual display
#' of possible problematic points and the ability to manually remove these points.
#' Setting `interactive = FALSE` will automatically remove these points from the data frame.
#' @inheritParams correct_class
#'
#' @examples
#' \donttest{
#' cleaned_data <- process_flagged(data, interactive = FALSE)
#' }
#'
#' @return Return cleaned data frame.
#'
#' @importFrom CoordinateCleaner clean_coordinates
#' @importFrom leaflet leaflet providers awesomeIcons addProviderTiles addAwesomeMarkers addMiniMap fitBounds removeMarker
#' @importFrom magrittr "%>%"
#'
#' @export

process_flagged <- function(df, interactive = TRUE, latitude = "latitude", longitude = "longitude",
                            scientific.name = "scientificName") {

  if (NROW(df) == 0) return(df)

  df <- basic_locality_clean(df, latitude = latitude, longitude = longitude,
                             remove.zero = FALSE, precision = FALSE, remove.skewed = FALSE)

  # workaround for clean_coordinates since it relies on rownames:
  # https://github.com/ropensci/CoordinateCleaner/issues/24
  rownames(df) <- 1:nrow(df)
  if (interactive) {
    df2 <- suppressWarnings(CoordinateCleaner::clean_coordinates(df,
                            lon = longitude, lat = latitude, species = scientific.name,
                            value = "spatialvalid"))
  }
  else {
    df <- suppressWarnings(CoordinateCleaner::clean_coordinates(df,
                           lon = "longitude", lat = "latitude", species = "scientificName",
                           value = "clean"))
    return(df)
  }

  # find the flagged points
  flagged <- df2[df2$.summary == "FALSE", ]
  # make new column with (latitude, longitude) and point number if there are any flagged points
  if (nrow(flagged) > 0) {
    flagged$index <- as.character(1:nrow(flagged))
    flagged$coordinates <- paste0("(", flagged[[latitude]], ", ", flagged[[longitude]], ")", ", point #", flagged$index)
  }
  else {
    message("No flagged points found.")
    return(df)
  }
  # make a map of the flagged points; hovering over points will show lat/long, clicking on points will show species name
  coordinates <- flagged$coordinates
  icons <- leaflet::awesomeIcons(icon = "fa-leaf", iconColor = "#3CB371", library = "fa", squareMarker = TRUE, markerColor = "lightgreen")
  map <- leaflet::leaflet(data = flagged) %>%
    leaflet::addProviderTiles(providers$OpenStreetMap) %>%
    leaflet::addAwesomeMarkers(lng = flagged[[longitude]], lat = flagged[[latitude]], icon = icons, popup = ~coordinates, label = flagged[[scientific.name]], layerId = ~index) %>%
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
      leaflet::fitBounds(lowlong, lowlat, highlong, highlat)
    print(map)
  }
  if (input == "N" | input == 'n') {
    message("To zoom in and out, you can click on the plus and minus icons, respectively.")
  }

  newdf <- df

  # prompt the user to choose whether to remove any points
  input <- readline(prompt = "Would you like to remove a rectangular region of points from the dataframe? Enter Y for yes or N for no. ")
  if (input == "Y" | input == 'y') {
    # find the region of points to remove
    message("To remove points, you will be prompted to enter the region from which to remove points from the dataframe. You may repeat this process as many times as you would like.")
    lowlong <- readline(prompt = "Please enter the lower bound for longitude: ")
    lowlong <- as.numeric(lowlong)
    highlong <- readline(prompt = "Please enter the upper bound for longitude: ")
    highlong <- as.numeric(highlong)
    lowlat <- readline(prompt = "Please enter the lower bound for latitude: ")
    lowlat <- as.numeric(lowlat)
    highlat <- readline(prompt = "Please enter the upper bound for latitude: ")
    highlat <- as.numeric(highlat)
    # find the flagged points in the region entered
    flaggedFiltered <- flagged[ flagged[[longitude]] >= lowlong &
                                flagged[[longitude]] <= highlong &
                                flagged[[latitude]] >= lowlat &
                                flagged[[latitude]] <= highlat , ]
    for (i in 1:nrow(flaggedFiltered)) {
      for (j in 1: nrow(flagged)) {
        if (flaggedFiltered[[latitude]][i] == flagged[[latitude]][j]
            & flaggedFiltered[[longitude]][i] == flagged[[longitude]][j]) {
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
      newdf <- newdf[ newdf[[longitude]] != flaggedFiltered[[longitude]][i] &
                      newdf[[latitude]] != flaggedFiltered[[latitude]][i] , ]
    }

    # find the number of points removed from the original dataframe, and display it
    pointsRemoved = NROW(df) - NROW(newdf)
    message(paste0(pointsRemoved, " points removed."))


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
      flaggedFiltered <- flagged[ flagged[[longitude]] >= lowlong &
                                    flagged[[longitude]] <= highlong &
                                    flagged[[latitude]] >= lowlat &
                                    flagged[[latitude]] <= highlat , ]
      for (i in 1:nrow(flaggedFiltered)) {
        for (j in 1: nrow(flagged)) {
          if (flaggedFiltered[[latitude]][i] == flagged[[latitude]][j]
              & flaggedFiltered[[longitude]][i] == flagged[[longitude]][j]) {
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
        newdf <- newdf[ newdf[[longitude]] != flaggedFiltered[[longitude]][i] &
                        newdf[[latitude]] != flaggedFiltered[[latitude]][i] , ]
      }
      # find the number of points removed and display it
      pointsRemoved = current - NROW(newdf)
      message(paste0(pointsRemoved, " points removed."))
    }
  }
  input <- readline(prompt = "Would you like to remove any individual points? Enter Y for yes or N for no. ")
  while (input == "Y" | input == 'y') {
    point_num <- readline(prompt = "Enter the point number: ")
    map <- map %>%
           leaflet::removeMarker(point_num)
    print(map)
    newdf <- newdf[ newdf[[longitude]] != flagged[[longitude]][as.integer(point_num)] &
                    newdf[[latitude]] != flagged[[latitude]][as.integer(point_num)] , ]
    input <- readline(prompt = "Would you like to remove additional individual points? Enter Y for yes or N for no. ")
  }
  # return the new dataframe with the removed points
  return(newdf)
}
