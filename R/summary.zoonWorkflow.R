
#' A function to summarize the output of a zoon workflow
#' 
#' The function currently just outputs the dimensions and a few other
#'   parts of a workflow. This could certainly be developed to include useful
#'   bit of information from each module type e.g. the extent of rasters, the
#'   type of model object etc.
#' 
#'@param workflow A zoon workflow object
#'
#'@return A list that summarises the workflow
#'@name summary.zoonWorkflow
#'@export

summary.zoonWorkflow <- function(workflow){

  # Extract a few parts of the list.
  processDF <- list.map(workflow$process.output, df)
  processRas <- list.map(workflow$process.output, ras)
  models <- list.map(workflow$model.output, model)

  # Summarise, mostly with dim.
  summary <- list(occurrence = lapply(workflow$occurrence.output, dim),
                  covariate = lapply(workflow$covariate.output, dim),
                  process = list(lapply(processDF, dim), 
                                lapply(processRas, dim)),
                  model = lapply(workflow$model.output, class),
                  output = lapply(workflow$report, class))
  summary
}
