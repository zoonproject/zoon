#' A function to print zoonCitation
#'
#' Prints a zoonCitation object to console
#'  giving easy access to module citations
#'
#' @param x object of class zoonCitation
#' @param \dots currently ignored
#'
#' @name print.zoonCitation
#' @method print zoonCitation
#' @export

print.zoonCitation <- function(x, ...) {
  cat("To cite the", x$name, "module in publications use:\n\n")

  cat(paste0(
    x$authors, " (", format(x$date_submitted, "%Y"), ")", ". ",
    x$title, ". ",
    x$note, ". ",
    "Available at: ", x$url, "\n\n"
  )
  )

  cat("A BibTeX entry for LaTeX users is:\n\n")

  cat(
    "@Manual{\n",
    paste0("  title = {", x$title, "}\n"),
    paste0("  author = {", gsub(", ", " and ", x$authors), "}\n"),
    paste0("  year = {", format(x$date_submitted, "%Y"), "}\n"),
    paste0("  note = {", x$note, "}\n"),
    paste0("  url = {", x$url, "}\n"),
    "}"
  )
}
