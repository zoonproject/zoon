#' BuildModule
#'
#' Turn a function in the namespace into a module. Will later add functions to
#' upload module to figshare etc. And add testing that the module name is
#' unique.
#'
#' @param object A function that will be made into a module file. It is good
#'   practice to ensure your function does not have the same name as a base
#'   function, another module, or other common functions.
#' @param dir The directory to put the module into (defaults to the working
#'   directory).
#' @param type A string that defines the type of module. Possible module types
#'   are occurrence, covariate, process, model, and output.
#' @param title A short description of the module.
#' @param description (required) A single string giving a full description of
#'   the module.
#' @param details (optional) A single string giving details of the module.
#' @param paras A list of the form list(parameterName = 'Parameter
#'   description.', anotherParameter = 'Another description.') This is required
#'   if the module takes non-default arguments
#' @param author (required) String giving the author(s) name(s)
#' @param email (required) String giving the correspondence address for the
#'   module (only give one address).
#' @param version (optional) Numeric giving the version number. Default 0.1.
#' @param dataType Character vector required for all module types except
#'   'covariate'. Indicates the types of data that this module works with.
#'   Values can be any of 'presence-only', 'presence/absence',
#'   'presence/background', 'abundance' or 'proportion'. For a occurrence model
#'   this should indicate the type of data that is returned and for other
#'   modules should indicate the type of data they will work with. If the module
#'   works with multiple types they can be supplied in a vector, e.g.
#'   c('presence-only', 'presence/absence')
#' @param check Logical indicating if the module should be run through checks
#'   once it has been built. Defaults to TRUE.
#'
#' @return Name of the module. AS a side effect outputs a .R file to the
#'   directory specified.
#' @name BuildModule
#' @import methods
#' @import testthat
#' @importFrom utils capture.output
#' @export

BuildModule <- function (object, type, dir=".", title = "", description = "",
                        details = "", author = "", email = "", version = 0.1,
                        paras = NULL, dataType = NULL, check = TRUE) {

  
  module_types <- c("occurrence",
                    "covariate",
                    "process",
                    "model",
                    "output")
  
  data_types <- c("presence-only",
                  "presence/absence",
                  "presence/background",
                  "abundance",
                  "proportion")
  
  # Check object is a function
  if (!is(object, "function"))
    stop("object must be a function")

  # Check type is known
  if (!tolower(type) %in% module_types) {
    stop ("type must be one of ",
          "'occurrence', 'covariate', 'process', 'model', 'output'")
  }

  # Check dataType is known
  if (is.null(dataType)) {
    if (type != "covariate")
      stop ("dataType is needed for all modules except covariate modules")
  } else {
    if (any(!dataType %in% data_types)) {
      stop ("dataType must be one of 'presence-only', 'presence/absence', ",
            "'presence/background', 'abundance' or 'proportion'")
    }
  }

  # Check only one email address is given
  if (length(email) > 1)
    stop ("Please only give one email address for correspondence")

  # Collapse multiple authors
  authors <- paste(author, collapse = ", ")

  # Remove trailing '/' from dir
  dir <- gsub("/$", "", dir)

  Writeable(dir)

  # Is all meta information provided.
  if (title == "" | description == "" | authors == "" | email == "") {
    complete <- FALSE
  } else {
    complete <- TRUE
  }

  # What are the default arguments for each module type
  defArgs <- list(
    occurrence = NULL,
    covariate = NULL,
    process = c(".data"),
    model = c(".df"),
    output = c(".model", ".ras")
  )


  missing <- !names(formals(object)) %in% names(paras)
  missingParas <- names(formals(object))[missing]

  # Are all parameters documented (excluding defualt, zoon internal parameters).
  # If not give a warning.
  if (any(!missingParas %in% defArgs[[type]])) {
    complete <- FALSE
  }
  
  if (!complete) {
    warning(paste(
      "Information not complete. All arguments must be filled and",
      "all parameters documented before uploading module to Zoon repository."
    ))
  }

  # To ensure consistancy make sure the default parameters
  # have been used. This also ensures the documentation
  # makes sense
  if (any(!defArgs[[type]] %in% names(formals(object)))) {
    warning(paste(
      "Your", type, "module does not contain the default arguements",
      paste("[", paste(defArgs[[type]], collapse = ", "), "]", sep = ""),
      "See the vignette Building modules for more details."
    ))
  }

  # Add param statements for default arguements
  if (any(names(paras) %in% defArgs[[type]])) {
    warning(
      "Parameter descriptions for defaults [", defArgs[[type]],
      "]", " ignored"
    )
  }

  paras <- AddDefaultParas(paras, type)

  type <- tolower(type)
  obj <- deparse(substitute(object))

  # Sort out parameter formating.
  paraNames <- names(paras)
  paraDocs <- paste(vapply(paraNames,
                           function(x) {
                             paste("\n#'\n#' @param", x, paras[x])
                             },
                           FUN.VALUE = ""),
                    collapse = "")

  # Roxygen2 uses @ as a tag. So have to double it.
  email <- gsub("@", "@@", email)

  # Add Data type if required
  if (!is.null(dataType)) {
    dataType <- paste0("\n#'\n#' @section Data type: ",
                       paste(dataType, collapse = ", "))
  }

  # Version number. his will be checked by the website
  # on submission
  if (!inherits(version, "numeric"))
    stop ("Version must be numeric")
  
  version <- paste("\n#'\n#' @section Version:", version)

  # Give this as the current date
  submitted <- paste("\n#'\n#' @section Date submitted: ", Sys.Date())

  docs <- paste0(
    "#' @name ", obj,
    "\n#'\n#' @title ", title,
    "\n#'\n#' @description ", description,
    "\n#'\n#' @details ", details,
    paraDocs,
    "\n#'\n#' @family ", type,
    "\n#'\n#' @author ", authors, ", ", "\\email{", email, "}",
    dataType,
    version,
    submitted
  )

  # get and format the source code
  src <- capture.output(dput(object, control = "useSource"))
  src[1] <- sprintf("%s <- %s", obj, src[1])
  src <- paste0(src, collapse = "\n")

  # get the file path and write to disk
  fpath <- paste0(dir, "/", obj, ".R")
  write(docs, file = fpath)
  cat(src, file = fpath, append = TRUE)

  # add a final line break
  cat("\n", file = fpath, append = TRUE)

  # Run checks if requested
  if (check) {
    cat("Starting checks...")
    test_module(fpath)
    cat("done\n")
  }

  return(obj)
}
