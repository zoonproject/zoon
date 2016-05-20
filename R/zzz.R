# things to do on loading the package
.onLoad <- function(libname, pkgname) {
  # set up a temporary directory for raster::getData calls
  if (is.null(getOption('rasterDataDir'))) {
    options(rasterDataDir = tempdir())
  }
}

