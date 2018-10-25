# things to do on loading the package
.onLoad <- function(libname, pkgname) {
  # set up a temporary directory for raster::getData calls
  if (is.null(getOption("rasterDataDir"))) {
    options(rasterDataDir = tempdir())
  }
  # set up the default modules repo
  if (is.null(getOption("zoonRepo"))) {
    options(zoonRepo = "https://raw.githubusercontent.com/zoonproject/modules")
  }
  # set up the default branch
  if (is.null(getOption("zoonRepoBranch"))) {
    options(zoonRepoBranch = "master")
  }
}
