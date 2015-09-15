# things to do on loading the package
.onLoad <- function(libname, pkgname) {
  # set up a temporary directory for raster::getData calls
  options(rasterDataDir = tempdir())
}

