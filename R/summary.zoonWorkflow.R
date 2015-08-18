
#' A function to summarize the output of a zoon workflow
#' 
#' The function currently just outputs the dimensions and a few other
#'   parts of a workflow. This could certainly be developed to include useful
#'   bit of information from each module type e.g. the extent of rasters, the
#'   type of model object etc.
#' 
#'@param object A zoon workflow object
#'@param \dots Other arguents to be passed to other methods. Currently ignored
#'@return A list that summarises the workflow
#'@name summary.zoonWorkflow
#'@export

summary.zoonWorkflow <- function (object, ...){

  # Extract a few parts of the list.

  # R CMD check dislikes this as df and ras look like unbound variables
  processDF <- list.map(object$process.output, df)
  processRas <- list.map(object$process.output, ras)
  models <- list.map(object$model.output, model)

  # Summarise, mostly with dim.
  summary <- list(occurrence = lapply(object$occurrence.output, dim),
                  covariate = lapply(object$covariate.output, dim),
                  process = list(lapply(processDF, dim), 
                                lapply(processRas, dim)),
                  model = lapply(object$model.output, class),
                  output = lapply(object$report, class))
  summary
}
