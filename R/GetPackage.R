#' Helper to install (if needed) and load a package
#' 
#' Given a package name, either as a string or object,
#' load the package if it exists, else install it from
#' CRAN and then load
#' 
#' @param package A character vector of packages to load
#' @return NULL
#' @importFrom utils install.packages
#' @examples GetPackage('gam')
#' @export

GetPackage <- function (package) {
  
  # Check pageage is a character
  if(!inherits(x = package, what = 'character')) stop('package must be a character')
  
  for(i in package){
    
    # try loading and install and load if that doesn't work
    if (!require(i,
                 character.only = TRUE)) {
      
      # otherwise use install.packages
      install.packages(i,
                       repos = "http://cran.rstudio.com")
  
      # now load the package
      library(i,
              character.only = TRUE)
    }
  }
}

