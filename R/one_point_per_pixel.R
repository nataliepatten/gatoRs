#' @title Spatial Correction - One point per pixel
#'
#' @description
#' The `one_point_per_pixel` function retains only one point per raster pixel. This function is useful
#' when creating present-absent models.
#'
#' @details
#' This function requires package raster and spatstat.geom.
#'
#' @param df Data frame of occurrence records.
#' @param raster Raster object which will be used for ecological niche comparisons.
#' @param resolution Default = 0.5. Options - 0.5, 2.5, 5, and 10 (in min of a degree). 0.5 min of a degree is equal to 30 arc sec.
#' @inheritParams correct_class
#' @inheritParams basic_locality_clean
#'
#' @examples
#' ready_data <- one_point_per_pixel(data)
#'
#' @return df is a data frame with only one point per pixel.
#' Information about the columns in the returned data frame can be found in the documentation for `gators_download()`.

#' @importFrom raster res
#' @importFrom spatstat.geom nndist
#'
#' @export

one_point_per_pixel <- function(df, raster = NA, resolution = 0.5, precision = TRUE, digits = 2,
                                longitude = "longitude", latitude = "latitude"){

  if (NROW(df) == 0) return(df)

  df <- basic_locality_clean(df, latitude = latitude, longitude = longitude, remove.zero = FALSE,
                             precision = precision, digits = digits, remove.skewed = FALSE)

  if(!inherits(raster, "RasterLayer")){
    if(resolution == 0.5){
      rasterResolution  <- 0.008333333
    }else if(resolution == 2.5){
      rasterResolution  <- 0.04166667
    }else if(resolution == 5){
      rasterResolution  <- 0.08333333
    }else if(resolution == 10){
      rasterResolution  <- 0.1666667
    }
  } else{
    rasterResolution  <- max(raster::res(raster))
  }

  while(min(spatstat.geom::nndist(df[, c(longitude,latitude)])) < rasterResolution){
    nnD <- spatstat.geom::nndist(df[,c(longitude,latitude)])
    df <- df[-(which(min(nnD) == nnD) [1]), ]
  }

  return(df)
}
