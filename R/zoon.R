
# _____________     ____________       ____________         ___________________
#  \  ______   \__ _\   ______  \     _\   ______  \       _\      ___  \______
#   \       /   \  \ \  \     \  \__ _\ \  \     \  \     _\ \  \/    \  \_____
#    \     /     \  \ \  \     \  \  \ \ \  \     \  \__ _\ \ \  \     \  \____
#     \   /______ \  \ \  \_____\  \  \ \ \  \_____\  \  \ \ \ \  \     \  \___
#      \___________\__\_\___________\__\_\_\___________\__\_\_\_\___________\__
#                      \____________\___\_\____________\___\_\_\____________\__
#                                        \____________\_____\_\____________\___
# SPECIES DISTRIBUTION MODELLING                             \____________\____
#
# A PROTOTYPE FOR REPRODUCIBLE, ACCESSIBLE & SHAREABLE SCIENTIFIC OUTPUTS IN R




# name a specific sha for an state of the modules repo, for tagged
# versions of zoon, or 'master' for non-tagged versions
# .sha <- 'b1544fbadaa21c41b4b51922ae73e44923b890ed'
.sha <- 'master'



#'Zoon: A package for comparing multple SDM models, good model diagnostics
#'      and better reproducibility
#'@name zoon
#'@docType package
#'@import assertthat raster rlist RCurl httr httpuv dismo

NULL


#' Run a full workflow.
#'
#' This is the main function of zoon. The arguments should specify at least five
#'   modules, at least one of each type.
#'   If modules do not have any arguments to be specific (or defaults are being
#'   used then simply give the names of the module. If arguments are needed 
#'   give the modules in the form of a function 
#'   e.g. occurrence = AModule(para1 = 2, para2 = 'detail')
#'
#'@param occurrence Occurrence module to be used.
#'@param covariate  Covariate module to be used.
#'@param process Process module to be used.
#'@param model SDM model module to be used. 
#'@param output Output module to be used.
#'@param forceReproducible Logical whether to force zoon to collect modules 
#'  from the online repo. This ensure the analysis is reproducible.
#'
#'@return A list with the results of each module and a copy of the
#'  code used to execute the workflow (what's there now should be source-able
#'  though I'm sure there is a much neater approach than the one I took - the
#'  ultimate aim would be a much nicer way of enhancing reproducibility).
#'@export
#'@name workflow
#'@examples 
#'# run a workflow, using the logistic regression model
#'#work1 <- workflow(occurrence = 'UKAnophelesPlumbeus',
#'#                 covariate = 'UKAir',
#'#                 process = 'OneHundredBackground',
#'#                 model = 'LogisticRegression',
#'#                 output = 'SameTimePlaceMap')
#'
#'#str(work1, 1)
#'
#'#work2 <- workflow('UKAnophelesPlumbeus', 'UKAir', 'OneHundredBackground',   
#'#           'RandomForest', 'PrintMap')
#'
#'
#'

workflow <- function(occurrence, covariate, process, model, output, forceReproducible=FALSE) {

  occSub <- substitute(occurrence)
  covSub <- substitute(covariate)
  proSub <- substitute(process)
  modSub <- substitute(model)
  outSub <- substitute(output)

  call <- SortArgs(PasteAndDep(occSub), PasteAndDep(covSub), PasteAndDep(proSub), 
            PasteAndDep(modSub), PasteAndDep(outSub), forceReproducible)
 
  # save the local environment as it needs to be passed to various functions.
  e <- environment() 

  # Check all modules are of same list structure
  occurrence.module <- CheckModList(occSub)
  covariate.module <- CheckModList(covSub)
  process.module <- CheckModList(proSub)
  model.module <- CheckModList(modSub)
  output.module <- CheckModList(outSub)
  
  # create a list of these things to return
  call.list <- list(occurrence.module,
                    covariate.module,
                    process.module,
                    model.module,
                    output.module)
  
  # Only one of occurrence, covariate, process and model can be a list of 
  #   multiple modules. But ignore chained modules.
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
  
  
  
  # Run the modules. (these functions are in DoModuleFunctions.R)
  # But we have to check for chained modules and deal with them
  # And work out which module has been given as a list, and lapply over that.

  # Each module is in trycatch.
  # If a module breaks we want to save the progress so far and let the user 
  # know which module broke.

  # First the data collection modules
  # Actually tryCatch here only tells user which module broke, nothing to save.

  # If you want to parallelise modules, (properly on multicores), lapply will
  #   become snowfall::sflapply or newer things.

  tryCatch({
    occurrence.output <- lapply(occurrenceName, function(x) do.call(x$func, x$paras))
    # Then bind together if the occurrence modules were chained
    if (identical(attr(occurrence.module, 'chain'), TRUE)){
      occurrence.output <- list(do.call(rbind, occurrence.output))
    }
    #return(occurrence.output)
  },  
    error = function(cond){
      ErrorAndSave(cond, 1, e)
    }
  )

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

  # Simply combine data into basic df shape
  # This shape is then input and output of all process modules.
  # Also makes it easy to implement a NULL process
  tryCatch({
    if(length(covariateName) > 1){    
      data <- lapply(covariate.output, 
                     function(x) ExtractAndCombData(occurrence.output[[1]], x))
    } else {
      data <- lapply(occurrence.output, 
                     function(x) ExtractAndCombData(x, covariate.output[[1]]))
    }
  },  
    error = function(cond){
      ErrorAndSave(cond, 3, e)
    }
  )



  # Do process modules
  
  tryCatch({  
    process.output <-  DoProcessModules(process.module, processName, data, e)
  },  
    error = function(cond){
      ErrorAndSave(cond, 3, e)
    }
  )
  
  
  # Model module
  tryCatch({
    model.output <- DoModelModules(model.module, modelName, process.output, e)
  },  
    error = function(cond){
      ErrorAndSave(cond, 4, e)
    }
  )    
  #output module
  # If output isn't chained, might have to lapply over 
  #   output, covariate or process
  # If output is chained, either covariate or process only. 
  #  Within this need to chain output
  tryCatch({
    output.output <- DoOutputModules(output.module, outputName, 
                       covariate.module, covariate.output, model.output, e)
  },  
    error = function(cond){
      ErrorAndSave(cond, 5, e)
    }
  )



  output <- list(occurrence.output = occurrence.output,
              covariate.output = covariate.output,
              process.output = process.output,
              model.output = model.output,
              report = output.output,
              call = call,
              call.list = call.list)

  class(output) <- 'zoonWorkflow'
  
  return(output)
}




