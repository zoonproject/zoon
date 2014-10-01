
# Helper to install (if needed) and load a package.
# if `github == TRUE` then package must be a string like:
# 'zoonproject/zoon'. Otherwise it's just the package name,
# either as a string or object.

GetPackage <- function (package,
                        github = FALSE) {
  
  # convert to string if it isn't already
  package <- as.character(substitute(package))
  
  # get devtools and chop up the path if it's on github
  if (github) {
    
    # get the whole path
    package_path <- package
    
    # now strip to after the slash so that `library` works
    package <- strsplit(package_path,
                              '/')[[1]][2]
    
  }
    

  # try loading and install and load if that doesn't work
  if (!require(package,
               character.only = TRUE)) {
    
    # if it's a github package
    if (github) {
      
      # load devtools (recursively calling GetPackage)
      GetPackage('devtools',
                 github = FALSE)
      
      install_github(package_path)
            
    } else {
      
      # otherwise use install.packages
      install.packages(package,
                       repos = "http://cran.ma.imperial.ac.uk/")
      
    }
    
    # now load the package
    library(package,
            character.only = TRUE)
  }
}

