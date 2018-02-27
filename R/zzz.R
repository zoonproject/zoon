# things to do on loading the package
.onLoad <- function(libname, pkgname) {
  
  # set up a temporary directory for raster::getData calls
  if (is.null(getOption("rasterDataDir"))) {
    options(rasterDataDir = tempdir())
  }
  
  # set up the default modules repo
  if (is.null(getOption("zoonModulesRepo"))) {
    options(zoonModulesRepo = "zoonproject/modules")
  }
  
  # set up the default branch
  if (is.null(getOption("zoonModulesBranch"))) {
    options(zoonModulesBranch = "dark_modules")
  }
  
  # check the module versions are up to date, and load
  sync_modules()
  
}
