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

  assert_that(is.string(module))

  helpURL <- paste0('https://raw.githubusercontent.com/zoonproject/modules/master/man/', module, '.Rd')
  txt <- getURL(helpURL, ssl.verifypeer=FALSE)
  helpFile <- paste0(tempdir(), '/',  module, '.Rd')
  writeLines(txt, helpFile)
  tools::Rd2txt(tools:::parse_Rd(helpFile))
}



