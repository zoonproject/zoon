#'ModuleHelp
#'
#'Returns the help file for a zoon module
#'Currently only text help is implemented.
#'
#'@param module The name of a zoon module (as a string)
#'
#'@return Prints the help page to screen.
#'@seealso \code{\link{GetModuleList}}
#'@name ModuleHelp
#'@export

ModuleHelp <- function(module){

  module <- as.character(substitute(module))
  assert_that(is.string(module))

  helpURL <- paste0('https://raw.githubusercontent.com/zoonproject/modules/master/man/', module, '.Rd')

  if(url.exists(helpURL)){
    txt <- getURL(helpURL, ssl.verifypeer=FALSE)
    helpFile <- paste0(tempdir(), '/',  module, '.Rd')
    writeLines(txt, helpFile)
    tools::Rd2txt(tools:::parse_Rd(helpFile))
  } else {
    modList <- GetModuleList()
    closeMatches <- modList[agrep(module, modList, max.distance = 0.3)]
    if(length(closeMatches) == 0){
      stop("Can't find '", module, "' or any modules with closely matching names.") 
    } else if (length(closeMatches) == 1){
      stop("Can't find '", module, "'. Did you mean '", closeMatches, "'?")
    } else {
      stop("Can't find '", module, "'. Did you mean one of '", paste(closeMatches, collapse="', "), "'?")
    }
  }
}




