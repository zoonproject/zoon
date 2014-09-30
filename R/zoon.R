
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
#'    modules, at least one of each type.
#'
#'@param occurrence The name of the module to be used to get occurence data
#'@param covariate  The name of the module to be used to get covariate 
#'  data.
#'@param process The name of the module to be used to process the data
#'@param model The name of the SDM model module to be used 
#'@param output The name of the module to be used to map output
#'
#'@return A list with the results of each module and a copy of the
#'  code used to execute the workflow (what's there now should be source-able
#'  though I'm sure there is a much neater approach than the one I took - the
#'  ultimate aim would be a much nicer way of enhancing reproducibility).
#'@export
#'@name workflow
#'@examples 

#'# run a workflow, using the logistic regression model
#'work1 <- workflow(occurMod = 'UKAnophelesPlumbeus',
#'                 covarMod = 'UKAir',
#'                 procMod = 'OneHundredBackground',
#'                 modelMod = 'LogisticRegression',
#'                 outMod = 'SameTimePlaceMap')
#'
#'str(work1, 1)
#'
#'work2 <- workflow('UKAnophelesPlumbeus', 'UKAir', 'OneHundredBackground', 
#'           'RandomForest', 'PrintMap')
#'
#'
#'

workflow <- function(occurrence, covariate, process, model, output) {
  
  # Check all modules are of same list structure
  occurrence.module <- CheckModList(occurrence)
  covariate.module <- CheckModList(covariate)
  process.module <- CheckModList(process)
  model.module <- CheckModList(model)
  output.module <- CheckModList(output)
  
  # Only one of occurrence, covariate, process and model can be a list of 
  #   mulitple modules.
  isChain <- sapply(list(occurrence, covariate, 
    process, model, output), function(x) identical(attr(x, 'chain'), TRUE))
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
  if (identical(attr(occurrence, 'chain'), TRUE)){
    occurrence.output <- list(do.call(rbind, occurrence.output))
  }

  covariate.output <- lapply(covariateName, function(x) do.call(x$func, x$paras))
  if (identical(attr(covariate, 'chain'), TRUE)){
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
  if (!identical(attr(process, 'chain'), TRUE)){
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

  if(!identical(attr(output, 'chain'), TRUE)){
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
  return(lapply(modules, function(x) list.append(func = GetModule(x$module), x)))
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



#'Turns named options into list ready to be used by workflow()
#'
#'@param module The module name or URL.
#'@param ... Any other parameters or options needed by that  module.
#'           All extra options must be named i.e. ModuleOptions('Name', x=1)
#'           Not ModuleOptions('Name', 1).
#'
#'
#'@return A list with all module options and the module name/URL in.
#'@name ModuleOptions
#'
#'@export
#'@examples print('No examples yet')

ModuleOptions <- function(module, ...){
  assert_that(is.string(module))
  options <- list(module=module, ...)
  if ('' %in% names(options)){
    stop(paste0('Unnamed options in module ', module, 
                ': All options must be named'))
  }
  options <- vector("list", 2)
  names(options) <- c('module', 'paras')
  options$module <- module
  options$paras <- list(...)
  
  return(options)
}



# Helper to check if module is in structure of ModuleOptions() list
# And convert otherwise.

CheckModStructure <- function(x){
  if (is.string(x)){
    x <- ModuleOptions(x)
  }
  return(x)
}

# Helper to install (if needed) and load a package.
# if `github == TRUE` then package must be a string like:
# 'zoonproject/zoon'. Otherwise it's just the package name,
# either as a string or object.

GetPackage <- function (package,
                        github = FALSE) {
  
  # convert to string if it isn't already
  package <- as.character(substitute(package))
  
  # get devtools and chop up the path if it's on github
  if (github) {
    
    # get the whole path
    package_path <- package
    
    # now strip to after the slash so that `library` works
    package <- strsplit(package_path,
                              '/')[[1]][2]
    
  }
    

  # try loading and install and load if that doesn't work
  if (!require(package,
               character.only = TRUE)) {
    
    # if it's a github package
    if (github) {
      
      # load devtools (recursively calling GetPackage)
      GetPackage('devtools',
                 github = FALSE)
      
      install_github(package_path)
            
    } else {
      
      # otherwise use install.packages
      install.packages(package,
                       repos = "http://cran.ma.imperial.ac.uk/")
      
    }
    
    # now load the package
    library(package,
            character.only = TRUE)
  }
}


# Helper to sort modules into lists.

CheckModList <- function(x){
  if (!is.list(x)){
    ModuleList <- list(CheckModStructure(x))
  } else if (identical(names(x), c('module', 'paras'))){
    ModuleList <- list(x)
  } else {
    ModuleList <- lapply(x, CheckModStructure)
  }
  
  return(ModuleList)
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




