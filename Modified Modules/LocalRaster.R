#' @title Covariate module: LocalRaster
#'
#' @description LocalRaster: Read in a single raster or a list of rasters and stack them.
#'
#' @details Read in a single raster or a list of rasters and stack them.
#'
#' @param filenames Either a string of the filename of the raster layer, or a list of strings of filenames to rasters that will be stacked. 
#'
#' @name LocalRaster
#' @family covariate
#' @author Tim Lucas \email{timcdlucas@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
LocalRaster <-
function(filenames='myRaster'){

  if(is.character(filenames) && length(filenames) == 1){
    ## Check if file exists
    if(file.exists(filenames)){
      ## Load raster from file
      raster <- raster(filenames)
    }else{
      ## Load raster from global environment
      raster <- eval(parse(text = filenames),envir = globalenv())
    }
  } else if(is.list(filenames)) {
    rasterList <- lapply(filenames, raster)
    raster <- stack(rasterList)
  }

  return(raster)
}
