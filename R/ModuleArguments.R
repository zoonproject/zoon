#' ModuleArguments
#'
#' Produce list of module arguments
#'
#' @param ModuleName string giving the name of the module
#' @return A list of arguments for user-intput to that module
#' @name ModuleArguments
#' @importFrom RCurl getURL
#' @export
#' @examples ModuleArguments('Background')

ModuleArguments <- function(ModuleName) {

  # Build URL
  ModuleURL <- sprintf(
    "%s/%s/R/%s.R",
    options("zoonRepo"),
    options("zoonRepoBranch"),
    ModuleName
  )
  
  # Check the URL exists
  if (!RCurl::url.exists(ModuleURL))
    stop("URL for module does not exist: ", ModuleURL)
  
  # Extract module function from webpage
  rawText <- RCurl::getURL(ModuleURL, ssl.verifypeer = FALSE)
  
  # Parse text from webpage
  txt <- parse(text = rawText)
  
  # Evaluate text in the workflow call environment
  eval(txt, envir = .GlobalEnv)
  
  all_arguments <- formals(ModuleName)
  
  arguments <- all_arguments[grepl('\\.', names(all_arguments)) == FALSE]
  arguments

}
