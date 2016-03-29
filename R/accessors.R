



#' Accessor functions for getting module outputs from a workflow object
#'
#' These functions access the output from each module type. If workflows
#'   are split using list, they will return a list with the output of 
#'   each seperate workflow being one element of the list. 
#'
#'@param workflow A workflow object
#'
#'@name occurrence
#'
#'@export
#'
#'@examples
#'\dontrun{
#'work1 <- workflow(occurrence = UKAnophelesPlumbeus,
#'                  covariate  = UKAir,
#'                  process    = OneHundredBackground,
#'                  model      = list(LogisticRegression, LogisticRegression),
#'                  output     = PrintMap)
#'
#'occurrence(work1)
#'covariate(work1)
#'process(work1)
#'model(work1)
#'model(work1)[[1]]
#'output(work1)
#'}

occurrence <- function(workflow){
  if(!inherits(workflow, 'zoonWorkflow')) stop('workflow should be a zoon workflow object.')


  if(length(workflow$occurrence.output) == 1){
    out <- workflow$occurrence.output[[1]]
  } else {
    out <- workflow$occurrence.output
  }
  out

}



#'@rdname occurrence
#'@name covariate
#'@export

covariate <- function(workflow){
  if(!inherits(workflow, 'zoonWorkflow')) stop('workflow should be a zoon workflow object.')

  
  if(length(workflow$covariate.output) == 1){
    out <- workflow$covariate.output[[1]]
  } else {
    out <- workflow$covariate.output
  }
  out

}



#'@rdname occurrence
#'@name process
#'@export

process <- function(workflow){
  if(!inherits(workflow, 'zoonWorkflow')) stop('workflow should be a zoon workflow object.')

  
  if(length(workflow$process.output) == 1){
    out <- workflow$process.output[[1]]
  } else {
    out <- workflow$process.output
  }
  out

}



#'@rdname occurrence
#'@name model
#'@export
model <- function(workflow){
  if(!inherits(workflow, 'zoonWorkflow')) stop('workflow should be a zoon workflow object.')

  
  if(length(workflow$model.output) == 1){
    out <- workflow$model.output[[1]]
  } else {
    out <- workflow$model.output
  }
  out

}


#'@rdname occurrence
#'@name output
#'@export
output <- function(workflow){
  if(!inherits(workflow, 'zoonWorkflow')) stop('workflow should be a zoon workflow object.')

  
  if(length(workflow$report) == 1){
    out <- workflow$report[[1]]
  } else {
    out <- workflow$report
  }
  out

}


