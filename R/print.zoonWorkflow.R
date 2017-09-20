#' A function to print a zoonWorkflow object
#'
#' The function returns a very simple output detailing the function call.
#'
#' @param x object of class zoonWorkflow
#' @param \dots currently ignored
#'
#' @name print.zoonWorkflow
#' @method print zoonWorkflow
#' @export

print.zoonWorkflow <- function(x, ...) {
  cat("zoonWorkflow Object\n===================\n\n")
  cat("Call:", x$call, "\n")
}
