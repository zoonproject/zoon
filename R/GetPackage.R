#' Helper to install (if needed) and load a package
#' 
#' Given a package name, either as a string or object,
#' load the package if it exists, else install it from
#' CRAN and then load
#' 
#' @param package The name of the package with or without
#' quotes
#' @return NULL
#' @importFrom utils install.packages
#' @examples  
#' \dontrun{
#' GetPackage('gam')
#' }
#' @export

GetPackage <- function (package) {
  
  # convert to string if it isn't already
  package <- as.character(substitute(package))
  
  # try loading and install and load if that doesn't work
  if (!require(package,
               character.only = TRUE)) {
    
    # otherwise use install.packages
    install.packages(package,
                     repos = "http://cran.rstudio.com")

    # now load the package
    library(package,
            character.only = TRUE)
  }
}

