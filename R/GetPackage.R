#' Helper to install (if needed) and load a package
#'
#' Given a package name, either as a string or object,
#' load the package if it exists, else install it from
#' CRAN and then load
#'
#' @param package A character vector of packages to load
#' @return NULL
#' @importFrom utils install.packages installed.packages
#' @export
#' @examples
#' \dontrun{
#' 
#' GetPackage('gam')
#' 
#' }

GetPackage <- function(package) {

  # Check package is a character
  if (!inherits(x = package, what = "character"))
    stop("package must be a character")

  for (i in package) {

    # if it isn't installed, install it
    installed_packages <- rownames(installed.packages())

    if (!i %in% installed_packages){
      
      cat(paste0('A module requires the package "',
                i,
                '". Would you like to install it?'))
      
      if(interactive()) {
        installChoice <- utils::menu(c("yes", "no"))
        if(installChoice == 1){
          install.packages(i, repos = "http://cran.rstudio.com")
        } else {
          stop('Not installing packages and cannot continue.' )
        }
      }
    }
    
    # now load the package
    library(i, character.only = TRUE)
    
  }
}
