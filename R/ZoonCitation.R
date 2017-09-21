#' ZoonCitation
#'
#' How to cite Zoon Modules in publications
#'
#' @param ModuleName string giving the name of the module
#' @return A \code{zoonCitation} object - A list of elements
#' that make up the citation
#' @name ZoonCitation
#' @seealso \code{\link{print.zoonCitation}}
#' @importFrom RCurl url.exists
#' @export

ZoonCitation <- function(ModuleName) {

  # Build the URL
  ModuleRepo <- "https://raw.githubusercontent.com/zoonproject/modules"
  ModuleURL <- paste0(ModuleRepo, "/master/R/", ModuleName, ".R")

  # Check the URL exists
  if (!RCurl::url.exists(ModuleURL))
    stop("URL for module does not exist: ", ModuleURL)

  # Parse the roxygen blocks
  ModBlocks <- ZoonModuleParse(ModuleURL)

  section_idx <- grepl("section", names(ModBlocks))
  version_idx <- grepl("^Version: ", ModBlocks[section_idx])
  submitted_idx <- grepl("^Date submitted", ModBlocks[section_idx])
  
  # Get the features needed for the citation
  version <- ModBlocks[section_idx][version_idx]
  title <- ModBlocks$title
  name <- ModBlocks$name
  authors <- gsub(", \\\\email\\{.+\\}", "", ModBlocks$author)
  date_section <- ModBlocks[section_idx][submitted_idx]
  date_submitted <- as.Date(gsub("^Date submitted: ", "", date_section))
  note <- paste("Zoon module", tolower(version))
  url <- ModuleURL
  email_index <- gregexpr("\\\\email\\{.+\\}", ModBlocks$author)[[1]]
  email <- substr(ModBlocks$author,
                  email_index[1] + 7,
                  email_index[1] + attr(email_index, "match.length") - 2)

  citation <- list(
    title = title,
    name = name,
    authors = authors,
    date_submitted = date_submitted,
    note = note,
    email = email,
    url = url
  )

  class(citation) <- "zoonCitation"

  return(citation)
}
