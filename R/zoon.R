
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


#' workhorse
#'
#'@param occurMod The name of the function (module) to be used to get occurence data
#'@param covariate.module  The name of the function (module) to be used to get covariate data
#'@param process.module The name of the function (module) to be used to process the data
#'@param model.module The name of the SDM model function (module) to be used 
#'@param output.module The name of the function (module) to be used to map output
#'
#'@return A list with the results of each module and a copy of the
#'       code used to execute the workflow (what's there now should be source-able
#'       though I'm sure there is a much neater approach than the one I took - the
#'       ultimate aim would be a much nicer way of enhancing reproducibility).
#'@export
#'@name workflow
#'@examples 

#'# run a workflow, using the logistic regression model
#'\dontrun{ans1 <- workflow(occurMod = 'AnophelesPlumbeus',
#'                 covarMod = 'AirNCEP',
#'                 procMod = 'OneHundredBackground',
#'                 modelMod = 'LogisticRegression',
#'                 outMod = 'SameTimePlaceMap')
#'
#'str(ans1, 1)
#'
#'
#'# plot the resulting maps
#'
#'plot(ans1$map,
#'     zlim = c(0, 1),
#'     main = 'LR')
#'
#'
#'points(ans1$occ[, 1:2],
#'       pch = 16,
#'       cex = 0.3)
#'}
#'

workflow <- function(occurMod,
                     covarMod,
                     procMod,
                     modelMod,
                     outMod) {
  
  # Before start workflow, need to define this function.
  # It's here as a hack to get it defined in right environment.
  # Will fix it another day.
  
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
    #'@param modelFunction String giving the name of the model function which is in turn the
    #'  name of the module.
    #'
    #'@param paras All other parameters that should be passed to the model function. 
    #'  i.e. model[[1]]$paras
    #'
    #'@return A list of length 2 containing the model trained on all data and a data.frame
    #'  which contains value, type, fold, lon, lat, predictions and then all environmental
    #'  variables.
    #'
    #'@name RunModels
    
    RunModels <- function(df, modelFunction, paras){
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
          modelFold <- do.call(modelFunction, c(df = list(df[df$fold != i, ]), paras))
          dfOut$predictions[df$fold == i] <- 
            predict(modelFold, newdata = df[df$fold == i, 6:NCOL(df), drop=FALSE], type = 'response') 
        }
      }
      
      # Run model on all data.
      m <- do.call(modelFunction, c(df = list(df), paras))
      
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
    
    
    
    
    # Check all modules are of same list structure
    
    # Check all modules are of same list structure
    occurrence.module <- CheckModList(occurMod)
    covariate.module <- CheckModList(covarMod)
    process.module <- CheckModList(procMod)
    model.module <- CheckModList(modelMod)
    output.module <- CheckModList(outMod)
    
    # Only one of occurrence, covariate, process and model can be a list of mulitple modules.
    isChain <- sapply(list(occurMod, covarMod, 
      procMod, modelMod, outMod), function(x) identical(attr(x, 'chain'), TRUE))
    NoOfModules <- sapply(list(occurrence.module, covariate.module, 
      process.module, model.module, output.module), length)
    if(sum(NoOfModules[!isChain] > 1) > 1){
      stop('Only one of occurrence, process and model modules can be a list of mulitple modules.')
    }
    
    
    # Get the modules (functions) from github. 
    # Save name of functions as well as load functions into global namespace.
    # Will probably want to make this so it checks namespace first.
    occurrence <- GetModules(occurrence.module) 
    covariate <- GetModules(covariate.module) 
    process <- GetModules(process.module) 
    # Check for val type lon lat covs
    model <- GetModules(model.module) 
    # Test for predict method
    output <- GetModules(output.module) 
    
    
    
    # Run the modules.
    # But we have to check for chained modules and deal with them
    # And work out which module has been given as a list, and lapply over that.


    # First the data collection modules
    occurrence.output <- lapply(occurrence, function(x) do.call(x$func, x$paras))
    # Then bind together if the occurrence modules were chained
    if (identical(attr(occurMod, 'chain'), TRUE)){
      occurrence.output <- list(do.call(rbind, occurrence.output))
    }

    covariate.output <- lapply(covariate, function(x) do.call(x$func, x$paras))
    if (identical(attr(covarMod, 'chain'), TRUE)){
      covariate.output <- list(do.call(raster::stack, covariate.output))
    }
    

    # Simply combine data into basic df shape
    # This shape is then input and output of all process modules.
    # Also makes it easy to implement a NULL process
    if(length(covariate) > 1){    
      data <- lapply(covariate.output, function(x) ExtractAndCombData(occurrence.output[[1]], x))
    } else {
      data <- lapply(occurrence.output, function(x) ExtractAndCombData(x, covariate.output[[1]]))
    }

    # We have to use lapply over a different list depending on whether occurrence,
    # covariate or process has multiple modules
    if (!identical(attr(procMod, 'chain'), TRUE)){
      if (length(process) > 1){
        process.output <- lapply(process, 
          function(x) do.call(x$func, 
                        c(list(data = data[[1]]), x$paras)))
      } else {
        process.output <- lapply(data,
          function(x) do.call(process[[1]]$func, 
                        c(list(data = x), process[[1]]$paras)))
      }
    } else { 
      # If procMod was chained, we must loop through the process modules applying them to
      # the output of the previous one.
      process.output <- data
      for(p in 1:length(process)){      
        process.output <- lapply(process.output,
          function(x) do.call(process[[p]]$func, 
                        c(list(data = x), process[[p]]$paras)))
      }
    }      
    
    
    
    # Model module
    if (length(model.module) > 1){
      model.output <- lapply(model, function(x) do.call(RunModels, 
                                                        list(df = process.output[[1]]$df, 
                                                          modelFunction = x$func, 
                                                          paras = x$paras)))
    } else {
      model.output <- lapply(process.output, function(x) do.call(RunModels, 
                                                                 list(df = x$df, 
                                                                   modelFunction = model[[1]]$func, 
                                                                    paras = model[[1]]$paras)))
    }
    
    #output module
    # If outMod isn't chained, might have to lapply over outputmod, covarmod or procmod
    # If outMod is chained, either covarmod or procmod only. Within this need to chain outmod

    if(!identical(attr(outMod, 'chain'), TRUE)){
      if (length(output.module) > 1){
        output.output <- lapply(output, 
                           function(x) do.call(x$func, 
                           c(list(model.output[[1]], covariate.output[[1]]), x$paras)))
      } else if (length(covariate.module) > 1){
        output.output <- lapply(covariate.output, 
                           function(x) do.call(output[[1]]$func, 
                           c(list(model.output[[1]], x), output[[1]]$paras)))    
      } else {
        output.output <- lapply(model.output, 
                           function(x) do.call(output[[1]]$func, 
                           c(list(x,  covariate.output[[1]]), output[[1]]$paras)))
      }
    } else {
      if (length(covariate.module) > 1){
        output.output <- lapply(covariate.output, 
                           function(y) lapply(output, 
                             function(x) do.call(x$func, 
                             c(list(model.output[[1]], y), x$paras))))    
      } else {
        output.output <- lapply(model.output, 
                           function(y) lapply(output, 
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
                output.output = output.output))
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
  zoonURL <- paste0('https://raw.githubusercontent.com/zoonproject/modules/master/R/', module, '.R')
  if (file.exists(module)){
    txt <- parse(text = paste(readLines(module), collapse="\n"))
  } else if (url.exists(zoonURL, .opts=list(ssl.verifypeer=FALSE))){
    txt <- parse(text = getURL(zoonURL, ssl.verifypeer=FALSE))
  } else if (url.exists(module, .opts=list(ssl.verifypeer=FALSE))){
    txt <- parse( text = getURL(module, ssl.verifypeer=FALSE))
  } else {
    stop(paste('Cannot find "', module, '". Check the URL or check that the module is at github.com/zoonproject'))
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
    stop(paste0('Unnamed options in module ', module, ': All options must be named'))
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
      install.packages(package)
      
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
#'Turns named options into list that are chained together rather than run in separate 
#'  analyses. Data modules the datasets are joined. Processes are run sequentially.
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
