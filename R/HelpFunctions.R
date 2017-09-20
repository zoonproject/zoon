#' ModuleHelp
#'
#' Returns the help file for a zoon module.
#'
#' @param module The name of a zoon module
#'
#' @return Prints the help page to screen.
#' @seealso \code{\link{GetModuleList}}
#' @name ModuleHelp
#' @export

ModuleHelp <- function(module) {
  module <- as.character(substitute(module))
  if (!is.character(module) | !length(module) == 1)
    stop("module must be a character of length 1")

  helpURL <- paste0("https://raw.githubusercontent.com/",
                    "zoonproject/modules/master/man/",
                    module, ".Rd")

  if (url.exists(helpURL)) {

    #     txt <- getURL(helpURL, ssl.verifypeer=FALSE)
    #     helpFile <- paste0(tempdir(), '/',  module, '.Rd')
    #     writeLines(txt, helpFile)
    #     tools::Rd2txt(tools::parse_Rd(helpFile))
    DisplayModuleHelp(helpURL)
  } else {
    modList <- GetModuleList()
    idx <- agrep(module, unlist(modList), max.distance = 0.3)
    closeMatches <- unlist(modList)[idx]
    if (length(closeMatches) == 0) {
      stop ("Can't find '", module,
            "' or any modules with closely matching names.")
    } else if (length(closeMatches) == 1) {
      stop ("Can't find '", module,
            "'. Did you mean '", closeMatches, "'?")
    } else {
      stop ("Can't find '", module,
            "'. Did you mean one of '",
            paste(closeMatches, collapse = "', "), "'?")
    }
  }
}


# Download a file with libcurl and no messages to the console
#' @importFrom utils download.file

DownloadQuietly <- function(url, file) {
  download.file(
    url,
    file,
    method = "libcurl",
    quiet = TRUE
  )
}



# Display a module helpfile in accordance with the 'help_type' option
# help is either displayed as an HTML or text, pdf is not supported and
# an error is returned
#' @importFrom utils browseURL

DisplayModuleHelp <- function(url) {

  # get Rd file
  f_raw <- tempfile()
  DownloadQuietly(url, f_raw)

  # get the help type
  types <- c("text", "html", "pdf")
  type <- getOption("help_type")
  if (is.null(type)) {
    type <- "text"
  }

  match.arg(tolower(type), types)

  # html case (opens in default viewer)
  if (type == "html") {

    # create a temporary help file and stick the html version in it
    tempDir <- tempfile()
    dir.create(tempDir)
    htmlFile <- file.path(tempDir, "index.html")
    htmlFile <- tools::Rd2HTML(
      f_raw,
      out = htmlFile
    )

    browser <- getOption("viewer")

    if (is.null(browser)) {

      # Produce a warning and continue using text
      warning(paste(
        "To display html help files (your default option) you",
        "need to specify a default viewer. Try changing your",
        "viewer via options. Printing to console instead", sep = "\n"
      ))

      temp <- tools::Rd2txt(f_raw)
    } else {

      # view the help file
      browseURL(
        url = htmlFile,
        browser = browser
      )
    }
  }

  if (type == "text") {

    # otherwise just dump to the console
    temp <- tools::Rd2txt(f_raw)
  }

  if (type == "pdf") {
    warning(paste(
      "pdf help files (your default option) cannot currently be displayed",
      "for zoon modules. Try changing your help viewing settings with:",
      'options(help_type = "html") or options(help_type = "text"). Printing',
      "to console instead", sep = "\n"
    ))
    temp <- tools::Rd2txt(f_raw)
  }

  invisible(NULL)
}
