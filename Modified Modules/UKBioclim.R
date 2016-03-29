#' @title Covariate module: UKBioclim
#'
#' @description Load Bioclim rasters at 5 degree resolution for the UK
#' 	(extent \code{c(-10, 10, 45, 65)}).
#'
#' @details The first time this module is used the data are downloaded from the web,
#' 	subsequently a local copy is used so it's much faster.
#' 	This is essentially a wrapper around the `raster` function `getData`,
#' 	see `?getData' for more details of what that function does.
#'	Use the \code{Bioclim} module for bioclim data with other extents.
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#'
#' @name UKBioclim
#' @family covariate
UKBioclim <-
  function() {
    world <- raster::getData('worldclim', var = 'bio', res = 5)
    uk <- crop(world, extent(-10, 10, 45, 65))
    return(uk)
  }
