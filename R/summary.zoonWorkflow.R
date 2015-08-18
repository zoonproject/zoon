#' A function to summarize the output of a zoon workflow
#' 
#' The function currently outputs the result of summary() on each
#' occurence element and each model element
#'
#'@param object A zoonWorkflow object
#'@param \dots currently ignored

#'@return A list of length 2, first the data summaries and then the 
#' model summaries. each of these is as long as the coressponding
#' elements in the zoonWorkflow object
#'@method summary zoonWorkflow
#'@name summary.zoonWorkflow
#'@export

summary.zoonWorkflow <- function(object, ...){

  # Create a list of the data summaries
  data_summary <- lapply(object$occurrence, summary)
  
  # Create a list of the model outputs
  model_summary <- lapply(object$model, function(x) summary(x$model))
  
  # Create object to return 
  summary <- list(data_summary = data_summary,
                  model_summary = model_summary)
  
  # Assign a class so that we can create a print method
  class(summary) <- 'zoonSummary'
  
  return(summary)
  
}
