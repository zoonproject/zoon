



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

occurrence <- function(workflow){
  if(!inherits(workflow, 'zoonWorkflow')) stop('workflow should be a zoon workflow object.')


  if(length(workflow$occurrence.output) == 1){
    out <- workflow$occurrence.output[[1]]
  } else {
    out <- workflow$occurrence.output
  }
  out

}

covariate <- function(workflow){
  if(!inherits(workflow, 'zoonWorkflow')) stop('workflow should be a zoon workflow object.')

  
  if(length(workflow$covariate.output) == 1){
    out <- workflow$covariate.output[[1]]
  } else {
    out <- workflow$covariate.output
  }
  out

}



process <- function(workflow){
  if(!inherits(workflow, 'zoonWorkflow')) stop('workflow should be a zoon workflow object.')

  
  if(length(workflow$process.output) == 1){
    out <- workflow$process.output[[1]]
  } else {
    out <- workflow$process.output
  }
  out

}



model <- function(workflow){
  if(!inherits(workflow, 'zoonWorkflow')) stop('workflow should be a zoon workflow object.')

  
  if(length(workflow$model.output) == 1){
    out <- workflow$model.output[[1]]
  } else {
    out <- workflow$model.output
  }
  out

}


output <- function(workflow){
  if(!inherits(workflow, 'zoonWorkflow')) stop('workflow should be a zoon workflow object.')

  
  if(length(workflow$report) == 1){
    out <- workflow$report[[1]]
  } else {
    out <- workflow$report
  }
  out

}


