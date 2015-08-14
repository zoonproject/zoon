#' A function to print zoon workflow summaries
#' 
#' Prints a simple zoonSummary object to console
#' 
#'@param x object of class zoonSummary
#'
#'@name print.Summary
#'@method print zoonSummary
#'@export

print.zoonSummary <- function(x, ...){
  
  cat('Data summaries\n==============\n')
#   if(class(x$data_summary) == 'list'){
#     print(do.call(rbind, x$data_summary))
#   } else {
#     print(unlist(x$data_summary))
#   }
  print(x$data_summary)
  cat('\n\nModel summaries\n===============\n\n')
  print(x$model_summary)
  
  
}