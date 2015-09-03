
# Helper to install (if needed) and load a package,
# given as the package name, either as a string or object.

GetPackage <- function (package) {
  
  # convert to string if it isn't already
  package <- as.character(substitute(package))
  
  # try loading and install and load if that doesn't work
  if (!require(package,
               character.only = TRUE)) {
    
    # otherwise use install.packages
    install.packages(package,
                     repos = "http://cran.ma.imperial.ac.uk/")

    # now load the package
    library(package,
            character.only = TRUE)
  }
}

