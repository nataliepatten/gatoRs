#' @title Spatial Correction - One point per pixel
#'
#' @description
#' The `one_point_per_pixel` retains only one point per pixel.
#'
#' @details
#' This function requires package raster and spatstat.geom.
#'
#' @param df Data frame of occurrence records.
#' @param raster Raster object which will be used for ecological niche comparisons.
#' @param resolution Default = 0.5. Options - 0.5, 2.5, 5, and 10 (in min of a degree). 0.5 min of a degree is equal to 30 arc sec.
#'
#' @return df is a data frame with only one point per pixel.
#'
#' @importFrom raster res
#' @importFrom spatstat.geom nndist
#'
#' @export

one_point_per_pixel <- function(df, raster = NA,
                                resolution = 0.5){
  if(is.na(raster) == TRUE){
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

  while(min(spatstat.geom::nndist(df[,15:14])) < rasterResolution){
    nnD <- spatstat.geom::nndist(df[,15:14])
    df <- df[-(which(min(nnD) == nnD) [1]), ]
  }

  return(df)
}
