#' Rerun a workflow object.
#'
#' Takes a workflow object and reruns it.
#'
#'@param workflow A zoonWorkflow object from a previous zoon analysis
#'@param from Which modules should be run. If NULL (default), run from the
#'  first NULL output (i.e. where the workflow broke). Otherwise takes an
#'  integer and runs from that module.
#'
#'@return A list with the results of each module and a copy of the
#'  call used to execute the workflow.
#'
#'@export
#'@name RerunWorkflow
#'@examples \dontrun{
#' w <- workflow(UKAnophelesPlumbeus, UKAir,
#'               OneHundredBackground, 
#'               LogisticRegression,
#'               SameTimePlaceMap)
#'
#' RerunWorkflow(w)
#'}



RerunWorkflow <- function(workflow, from = NULL) {
  
  assert_that(inherits(workflow, 'zoonWorkflow'))

  # If from isn't NULL, it should be an integer 1:5
  if (!is.null(from)){
    assert_that(from %in% c(1:5) )
  }

  # Find first NULL modules and run from there.
  if (is.null(from)) {
    NullModules <- sapply(list.subset(workflow, c(1:5)), is.null)
    if (!sum(NullModules) == 0){
      from <- which.max(NullModules)
    } else {
      from <- 1
    }
  }
  
  # get the arguments from the call used to run this workflow
  callArgs <- SplitCall(workflow$call)

  occSub <- callArgs['occurrence']
  covSub <- callArgs['covariate']
  proSub <- callArgs['process']
  modSub <- callArgs['model']
  outSub <- callArgs['output']

  forceReproducible <- as.logical(callArgs['forceReproducible'])

  # save the local environment as it needs to be passed to various functions.
  e <- environment() 

  # Check all modules are of same list structure
  occurrence.module <- CheckModList(occSub)
  covariate.module <- CheckModList(covSub)
  process.module <- CheckModList(proSub)
  model.module <- CheckModList(modSub)
  output.module <- CheckModList(outSub)
  
  # Only one of occurrence, covariate, process and model can be a list of 
  #   multiple modules.
  isChain <- sapply(list(occurrence.module, covariate.module, 
                         process.module, model.module, output.module), 
                    function(x) identical(attr(x, 'chain'), TRUE))
  NoOfModules <- sapply(list(occurrence.module, covariate.module, 
    process.module, model.module, output.module), length)
  if(sum(NoOfModules[!isChain] > 1) > 1){
    stop('Only one module type can be a list of multiple modules.')
  }
  
  
  
  # Get the modules (functions) from github. 
  # Save name of functions as well as load functions into global namespace.
  # Will probably want to make this so it checks namespace first.
  occurrenceName <- LapplyGetModule(occurrence.module, forceReproducible) 
  covariateName <- LapplyGetModule(covariate.module, forceReproducible) 
  processName <- LapplyGetModule(process.module, forceReproducible) 
  # Check for val type lon lat covs
  modelName <- LapplyGetModule(model.module, forceReproducible) 
  # Test for predict method
  outputName <- LapplyGetModule(output.module, forceReproducible) 
  
  
  
  
  # Different to workflow(), We have an if statement before each module is run
  #   to check the 'from' argument. 

  # Run the modules. (these functions are in DoModuleFunctions.R)
  # But we have to check for chained modules and deal with them
  # And work out which module has been given as a list, and lapply over that.

  # Each module is in trycatch.
  # If a module breaks we want to save the progress so far and let the user 
  # know which module broke.

  # First the data collection modules
  # Actually tryCatch here only tells user which module broke, nothing to save.
  if (from <= 1) {
    tryCatch({
      occurrence.output <- lapply(occurrenceName, function(x) do.call(x$func, x$paras))
      # Then bind together if the occurrence modules were chained
      if (identical(attr(occurrence.module, 'chain'), TRUE)){
        occurrence.output <- list(do.call(rbind, occurrence.output))
      }
    },  
      error = function(cond){
        ErrorAndSave(cond, 1, e)
      }
    )
  } else {
    occurrence.output <- workflow$occurrence.output
  }

  if (from <= 2) {
    tryCatch({
      covariate.output <- lapply(covariateName, function(x) do.call(x$func, x$paras))
      if (identical(attr(covariate.module, 'chain'), TRUE)){
        covariate.output <- list(do.call(raster::stack, covariate.output))
      }
    },  
      error = function(cond){
        ErrorAndSave(cond, 2, e)
      }
    )
  } else {
    covariate.output <- workflow$covariate.output
  }


  # Simply combine data into basic df shape
  # This shape is then input and output of all process modules.
  # Also makes it easy to implement a NULL process
  
  if(length(covariateName) > 1){    
    data <- lapply(covariate.output, 
                   function(x) ExtractAndCombData(occurrence.output[[1]], x))
  } else {
    data <- lapply(occurrence.output, 
                   function(x) ExtractAndCombData(x, covariate.output[[1]]))
  }



  if (from <= 3) {
    tryCatch({  
      process.output <-  DoProcessModules(process.module, processName, data, e)
    },  
      error = function(cond){
        ErrorAndSave(cond, 3, e)
      }
    )
  } else {
    process.output <- workflow$process.output
  }

  
  # Model module
  if (from <= 4) {
    tryCatch({
      model.output <- DoModelModules(model.module, modelName, process.output, e)
    },  
      error = function(cond){
        ErrorAndSave(cond, 4, e)
      }
    )    
  } else {
    model.output <- workflow$model.output
  }
  #output module
  # If output isn't chained, might have to lapply over 
  #   output, covariate or process
  # If output is chained, either covariate or process only. 
  #  Within this need to chain output

  if (from <= 5) {
    tryCatch({
      output.output <- DoOutputModules(output.module, outputName, 
                         covariate.module, covariate.output, model.output, e)
    },  
      error = function(cond){
        ErrorAndSave(cond, 5, e)
      }
    )
  } else {
    output.output <- workflow$output.output
  }


  # Collate output
  output <- list(occurrence.output = occurrence.output,
              covariate.output = covariate.output,
              process.output = process.output,
              model.output = model.output,
              report = output.output,
              call = workflow$call,
              call.list = workflow$call.list)

  class(output) <- 'zoonWorkflow'
  
  return(output)
}




