#' A function to print zoon workflow summaries
#' 
#' Prints a simple zoonSummary object to console
#' 
#'@param x object of class zoonSummary
#'@param \dots currently ignored

#'
#'@name print.Summary
#'@method print zoonSummary
#'@export

print.zoonSummary <- function(x, ...){
  
  cat('Data summaries\n==============\n')
  print(x$data_summary)

    cat('\n\nModel summaries\n===============\n\n')
  print(x$model_summary)
  
}