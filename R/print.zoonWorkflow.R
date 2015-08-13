# Methods

#' @method print zoonWorkflow
#' @export

print.zoonWorkflow <- function(x, ...){
  
  cat('zoonWorkflow Object\n===================\n\n')
  cat('Call:', x$call)
  
}