
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
#'   ___________________       ______________________________
#'   |                 |       |                            |
#'   | Occurrence data |       |  Environmental Covariates  |
#'   |_________________|       |____________________________|
#'            |                                |
#'            |________________________________|
#'                            |
#'                           \_/
#'                     _______________   
#'                     |             |
#'                     |  Processes  |
#'                     |_____________|
#'                            |
#'                           \_/
#'                     _______________   
#'                     |             |
#'                     |    Models   |
#'                     |_____________|
#'                            |
#'                           \_/
#'                     _______________   
#'                     |             |
#'                     |    Output   |
#'                     |_____________|
#'
#'
#'@param occurrence Occurrence module to be used.
#'@param covariate  Covariate module to be used.
#'@param process Process module to be used.
#'@param model SDM model module to be used. 
#'@param output Output module to be used.
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

workflow <- function(occurrence, covariate, process, model, output) {

  occSub <- substitute(occurrence)
  covSub <- substitute(covariate)
  proSub <- substitute(process)
  modSub <- substitute(model)
  outSub <- substitute(output)

  call <- sortArgs(deparse(occSub), deparse(covSub), deparse(proSub), 
                   deparse(modSub), deparse(outSub))

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
  occurrenceName <- GetModules(occurrence.module) 
  covariateName <- GetModules(covariate.module) 
  processName <- GetModules(process.module) 
  # Check for val type lon lat covs
  modelName <- GetModules(model.module) 
  # Test for predict method
  outputName <- GetModules(output.module) 
  
  
  
  # Run the modules.
  # But we have to check for chained modules and deal with them
  # And work out which module has been given as a list, and lapply over that.

  # Each module is in trycatch.
  # If a module breaks we want to save the progress so far and let the user 
  # know which module broke.

  # First the data collection modules
  # Actually tryCatch here only tells user which module broke, nothing to save.
  tryCatch({
    occurrence.output <- lapply(occurrenceName, function(x) do.call(x$func, x$paras))
    # Then bind together if the occurrence modules were chained
    if (identical(attr(occurrence.module, 'chain'), TRUE)){
      occurrence.output <- list(do.call(rbind, occurrence.output))
    }
  },  
    error = function(cond){
      
      w <- list(occurrence.output = NULL,
             covariate.output = NULL,
             process.output = NULL,
             model.output = NULL,
             report = NULL,
             call = call)
      assign('tmpZoonWorkflow', w,  env = .GlobalEnv)

      message('Caught errors:\n',  cond)
      message()
      message("Workflow progress stored in object 'tmpZoonWorkflow'.")
      x <- paste("Stopping workflow as error in Occurrence module.\n", 
                 "Workflow progress stored in object 'tmpZoonWorkflow'.")
      stop(x, call. = FALSE)
    }
  )

  tryCatch({
    covariate.output <- lapply(covariateName, function(x) do.call(x$func, x$paras))
    if (identical(attr(covariate.module, 'chain'), TRUE)){
      covariate.output <- list(do.call(raster::stack, covariate.output))
    }
  },  
    error = function(cond){
      
      w <- list(occurrence.output = occurrence.output,
             covariate.output = NULL,
             process.output = NULL,
             model.output = NULL,
             report = NULL,
             call = call)
      assign('tmpZoonWorkflow', w,  env = .GlobalEnv)

      message('Caught errors:\n',  cond)
      message()
      message("Workflow progress stored in object 'tmpZoonWorkflow'.")
      x <- paste("Stopping workflow as error in Covariate module.\n", 
                 "Workflow progress stored in object 'tmpZoonWorkflow'.")
      stop(x, call. = FALSE)
    }
  )

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

  # Have to use lapply over a different list depending on whether occurrence,
  # covariate or process has multiple modules

  #process.output <- RunProcessModules(data, process, processName)
  tryCatch({
    if (!identical(attr(process.module, 'chain'), TRUE)){
      if (length(processName) > 1){
        process.output <- lapply(processName, 
          function(x) do.call(x$func, 
                        c(list(data = data[[1]]), x$paras)))
      } else {
        process.output <- lapply(data,
          function(x) do.call(processName[[1]]$func, 
                        c(list(data = x), processName[[1]]$paras)))
      }
    } else { 
      # If process was chained, we must loop through the process modules 
      #   applying them to the output of the previous one.
      process.output <- data
      for(p in 1:length(processName)){      
        process.output <- lapply(process.output,
          function(x) do.call(processName[[p]]$func, 
                        c(list(data = x), processName[[p]]$paras)))
      }
    }      
  },  
    error = function(cond){
      
      w <- list(occurrence.output = occurrence.output,
             covariate.output = covariate.output,
             process.output = NULL,
             model.output = NULL,
             report = NULL,
             call = call)
      assign('tmpZoonWorkflow', w,  env = .GlobalEnv)

      message('Caught errors:\n',  cond)
      message()
      message("Workflow progress stored in object 'tmpZoonWorkflow'.")
      x <- paste("Stopping workflow as error in Proces module.\n", 
                 "Workflow progress stored in object 'tmpZoonWorkflow'.")
      stop(x, call. = FALSE)
    }
  )
  
  
  # Model module
  tryCatch({
    if (length(model.module) > 1){
      model.output <- 
        lapply(modelName, 
               function(x) 
                 do.call(RunModels,
                         list(df = process.output[[1]]$df, 
                              modelFunction = x$func, 
                              paras = x$paras,
                              workEnv = environment(eval(parse(text = modelName[[1]]$func)))
                             )
                        )
              )
    } else {
      model.output <- 
        lapply(process.output,
               function(x) 
                 do.call(RunModels, 
                         list(df = x$df, 
                              modelFunction = modelName[[1]]$func, 
                              paras = modelName[[1]]$paras,
                              workEnv = environment(eval(parse(text = modelName[[1]]$func)))
                             )
                        )
              )
    }
  },  
    error = function(cond){
      
      w <- list(occurrence.output = occurrence.output,
             covariate.output = covariate.output,
             process.output = process.output,
             model.output = NULL,
             report = NULL,
             call = call)
      assign('tmpZoonWorkflow', w,  env = .GlobalEnv)

      message('Caught errors:\n',  cond)
      message()
      message("Workflow progress stored in object 'tmpZoonWorkflow'.")
      x <- paste("Stopping workflow as error in Model module.\n", 
                 "Workflow progress stored in object 'tmpZoonWorkflow'.")
      stop(x, call. = FALSE)
    }
  )    
  #output module
  # If output isn't chained, might have to lapply over 
  #   output, covariate or process
  # If output is chained, either covariate or process only. 
  #  Within this need to chain output
  tryCatch({
    if(!identical(attr(output.module, 'chain'), TRUE)){
      if (length(output.module) > 1){
        output.output <- lapply(outputName, 
                           function(x) do.call(x$func, 
                           c(list(model.output[[1]], 
                                  covariate.output[[1]]),
                             x$paras)))
      } else if (length(covariate.module) > 1){
        output.output <- lapply(covariate.output, 
                           function(x) do.call(outputName[[1]]$func, 
                           c(list(model.output[[1]], x),
                             outputName[[1]]$paras)))    
      } else {
        output.output <- lapply(model.output, 
                           function(x) do.call(outputName[[1]]$func, 
                           c(list(x, covariate.output[[1]]), outputName[[1]]$paras)))
      }
    } else {
      if (length(covariate.module) > 1){
        output.output <- lapply(covariate.output, 
                           function(y) lapply(outputName, 
                             function(x) do.call(x$func, 
                             c(list(model.output[[1]], y), x$paras))))    
      } else {
        output.output <- lapply(model.output, 
                           function(y) lapply(outputName, 
                             function(x) do.call(x$func, 
                             c(list(y, covariate.output[[1]]), x$paras))))    
      }
    }
  },  
    error = function(cond){
      
      w <- list(occurrence.output = occurrence.output,
             covariate.output = covariate.output,
             process.output = process.output,
             model.output = model.output,
             report = NULL,
             call = call)
      assign('tmpZoonWorkflow', w,  env = .GlobalEnv)

      message('Caught errors:\n',  cond)
      message()
      message("Workflow progress stored in object 'tmpZoonWorkflow'.")
      x <- paste("Stopping workflow as error in Output module.\n", 
                 "Workflow progress stored in object 'tmpZoonWorkflow'.")
      stop(x, call. = FALSE)
    }
  )

  output <- list(occurrence.output = occurrence.output,
              covariate.output = covariate.output,
              process.output = process.output,
              model.output = model.output,
              report = output.output,
              call = call)

  class(output) <- 'zoonWorkflow'
  
  return(output)
}




