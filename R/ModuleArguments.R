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
  
  # Evaluate text in this function environment
  Module <- eval(txt)
  
  # Extract arguments
  all_arguments <- formals(Module)
  
  # Make sure all arguments display as character
  all_arguments_character <- lapply(all_arguments, deparse)
  
  # Remove the default arguments
  arguments <- all_arguments_character[grepl('^\\.', names(all_arguments_character)) == FALSE]
  arguments

}
