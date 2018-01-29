#' ModuleArguments
#'
#' Produce list of module arguments
#'
#' @param module string giving the name of the module
#' @return A list of arguments to that module
#' @name ModuleArguments
#' @importFrom RCurl getURL
#' @export

ModuleArguments <- function(module) {

  zoonURL <- sprintf(
    "%s/%s/R/%s.R",
    options("zoonRepo"),
    options("zoonRepoBranch"),
    module
  )
  
  rawText <- getURL(zoonURL, ssl.verifypeer = FALSE)
  
  # Parse text from webpage.
  txt <- parse(text = rawText)
  
  # Evaluate text in the workflow call environment
  eval(txt, envir = .GlobalEnv)
  
  arguments <- formals(module)
  
  arguments[grepl('\\.', names(arguments)) == FALSE]

}
