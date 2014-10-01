
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


  # First the data collection modules
  occurrence.output <- lapply(occurrenceName, function(x) do.call(x$func, x$paras))
  # Then bind together if the occurrence modules were chained
  if (identical(attr(occurrence.module, 'chain'), TRUE)){
    occurrence.output <- list(do.call(rbind, occurrence.output))
  }

  covariate.output <- lapply(covariateName, function(x) do.call(x$func, x$paras))
  if (identical(attr(covariate.module, 'chain'), TRUE)){
    covariate.output <- list(do.call(raster::stack, covariate.output))
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

  # Have to use lapply over a different list depending on whether occurrence,
  # covariate or process has multiple modules

  #process.output <- RunProcessModules(data, process, processName)
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
  
  
  
  # Model module
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
  
  #output module
  # If output isn't chained, might have to lapply over 
  #   output, covariate or process
  # If output is chained, either covariate or process only. 
  #  Within this need to chain output

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


  # get the command used to call this function
  bits <- sys.call()
  call <- paste0(bits[1],
                 '(', 
                 paste(bits[-1],
                       collapse = ', '),
                 ')')
  
  
  
  return(list(occurrence.output = occurrence.output,
              covariate.output = covariate.output,
              process.output = process.output,
              model.output = model.output,
              report = output.output,
              call = call))
}



#'A function to get a module function.
#'It assumes the module is in github.com/zoonproject unless it is a full url
#'Assigns the function to the global environment.
#'
#'@param module A string that describes the location of the R file. Can be a
#'      module name assuming the module is in github.com/zoonproject/modules.
#'      Otherwise can be a full URL or a local file.
#'@param type String describing the type of module. Only needed if getting from
#'  zoon github.
#'
#'@return Name of the function. Adds function to global namespace.
#'@name GetModule


GetModule <- function(module){
  zoonURL <- 
    paste0('https://raw.githubusercontent.com/zoonproject/modules/master/R/',
           module, '.R')
  if (file.exists(module)){
    txt <- parse(text = paste(readLines(module), collapse="\n"))
  } else if (url.exists(zoonURL, .opts=list(ssl.verifypeer=FALSE))){
    txt <- parse(text = getURL(zoonURL, ssl.verifypeer=FALSE))
  } else if (url.exists(module, .opts=list(ssl.verifypeer=FALSE))){
    txt <- parse( text = getURL(module, ssl.verifypeer=FALSE))
  } else {
    stop(paste('Cannot find "', module, 
      '". Check the URL or check that the module is at github.com/zoonproject'))
  }
  # Probably do one environment up (i.e. in workflow environment)
  eval(txt, envir = parent.frame(4))
  #eval(txt, envir = globalenv())
  eval(txt)
  new.func.name <- ls()[!ls() %in% c('module', 'txt', 'zoonURL')]
  return(new.func.name)
}


#' A function to apply GetModule to a list correctly.
#'@param modules A list from CheckModList() given details for 
#'  one or more modules.
#'@param type String describing the type of module. Only needed if getting from
#'  zoon github.
#'@name GetModules

GetModules <- function(modules){
  return(lapply(modules, function(x) list.append(func = GetModule(as.character(x$module)), x)))
}


#'RunModels
#'
#' A function to train and predict crossvalidation folds
#' and train one full model and predict any external validation data.
#'
#'@param df Dataframe from process module. Should contain columns 
#'  value, type, lon, lat
#'  and fold, a number indicating which cross validation set a datapoint is in.
#'  If fold is 0 then this is considered external validation data
#'  If all data is 0 or 1, then no cross validation is run.
#'    
#'@param modelFunction String giving the name of the model function which is 
#'  in turn the
#'  name of the module.
#'
#'@param paras All other parameters that should be passed to the model function. 
#'  i.e. model[[1]]$paras
#'
#'@param workEnv The environment name of the workflow call environment.
#'
#'@return A list of length 2 containing the model trained on all data and a
#'  data.frame which contains value, type, fold, lon, lat, predictions and 
#'  then all environmental variables.
#'
#'@name RunModels

RunModels <- function(df, modelFunction, paras, workEnv){
  # Count non zero folds
  # 0 are for external validation only. 
  k <- length(unique(df$fold)[unique(df$fold) != 0])
  
  # Init. output dataframe with predictions column
  dfOut <- cbind(df[, 1:5], predictions = NA, df[,6:NCOL(df)])
  names(dfOut)[7:ncol(dfOut)] <- names(df)[6:ncol(df)]
  
  # We don't know that they want cross validation.
  # If they do, k>1, then run model k times and predict out of bag
  # Skip otherwise
  if(k > 1){
    for(i in 1:k){
      modelFold <- do.call(modelFunction, c(df = list(df[df$fold != i, ]), 
                                            paras),
                           envir = workEnv)
      dfOut$predictions[df$fold == i] <- 
        predict(modelFold, newdata = df[df$fold == i, 6:NCOL(df), drop=FALSE], 
                type = 'response') 
    }
  }
  
  # Run model on all data.
  m <- do.call(modelFunction, c(df = list(df), paras), 
               envir = workEnv)
  
  # If external validation dataset exists, predict that;.
  if(0 %in% df$fold){
    dfOut$predictions[df$fold == 0] <- 
      predict(m, newdata = df[df$fold == 0, ], type = 'response')
  }
  
  # Return list of crossvalid and external validation predictions 
  # This list is then the one list that has everything in it.
  out <- list(model = m, data = dfOut)
  return(out)
}






# Helper to sort modules into lists.

CheckModList <- function(x){
  if (class(x) == 'name'){
    ModuleList <- list(list(module = as.character(x), paras = list()))
  } else if (x[[1]] == 'list') {
    listCall <- as.list(x)
    listCall[[1]] <- NULL
    ModuleList <- lapply(listCall, FormatModuleList)
  } else if (x[[1]] == 'Chain'){
    listCall <- as.list(x)
    listCall[[1]] <- NULL
    ModuleList <- lapply(listCall, FormatModuleList) 
    attr(ModuleList, 'chain') <- TRUE
  } else {
    paras <- as.list(x)
    paras[[1]] <- NULL
    ModuleList <- list(list(module = as.character(x[[1]]), paras = paras))
  }
  
  return(ModuleList)
}

# Little helper to format module lists
FormatModuleList <- function(x){
  listx <- as.list(x)
  newList <- list()
  newList$module <- listx[[1]]
  listx[[1]] <- NULL
  newList$paras <- listx
  return(newList)
}




# Simply extract covariates from rasters and combine with occurrence.
ExtractAndCombData <- function(occurrence, ras){

  noccurrence <- nrow(occurrence)
  
  # extract covariates
  occ_covs <- as.matrix(raster::extract(ras, occurrence[, c('longitude', 'latitude')]))
  
  # combine with the occurrence data
  df <- cbind(occurrence,
                   occ_covs)
  
  names(df)[6:ncol(df)] <- names(ras)
  
  return(list(df=df, ras=ras))
  
}


#'Chain
#'
#'Turns named options into list that are chained together rather than  
#'  run in separate analyses. Data modules the datasets are joined. 
#'  Processes are run sequentially.
#'
#'@param ... List of modules to be chained.
#'
#'@name Chain
#'
#'@export


Chain <- function(...){
  chain <- list(...)
  attr(chain, 'chain') <- TRUE
  return(chain)
}




