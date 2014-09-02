
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
#'@import assertthat raster


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

#install.packages('devtools')
#library(devtools)

#install_github('zoonproject/zoon')
#library(zoon)


workflow <- function(occurMod,
                     covarMod,
                     procMod,
                     modelMod,
                     outMod) {
 
  # Check all modules are of same list structure

  # Check all modules are of same list structure
  occurrence.module <- CheckModList(occurMod)
  covariate.module <- CheckModList(covarMod)
  process.module <- CheckModList(procMod)
  model.module <- CheckModList(modelMod)
  output.module <- CheckModList(outMod)
  
  # Only one of occurrence, covariate, process and model can be a list of mulitple modules.
  NoOfModules <- sapply(list(occurrence.module, covariate.module, process.module, model.module, output.module), length)
  if(sum(NoOfModules > 1) > 1){
    stop('Only one of occurrence, process and model modules can be a list of mulitple modules.')
  }
  

  # Get the modules (functions) from github. 
  # Save name of functions as well as load functions into global namespace.
  # Will probably want to make this so it checks namespace first.
  occurrence <- lapply(occurrence.module, function(x) GetModule(x$module))
  covariate <- lapply(covariate.module, function(x) GetModule(x$module))
  process <- lapply(process.module, function(x) GetModule(x$module))
  # Check for val type lon lat covs
  model <- lapply(model.module, function(x) GetModule(x$module))
  # Test for predict method
  output <-  lapply(output.module, function(x) GetModule(x$module)) 


  # stack(lapply(covariate.module, function(x) do.call(x[[1]], x[-1]))) Not needed
  # now but is basis for CovarDaisyChain()

  # Run the modules.
  # First the data collection modules
  occurrence.output <- lapply(occurrence.module, function(x) do.call(x[[1]], x[-1]))
  covariate.output <- lapply(covariate.module, function(x) do.call(x[[1]], x[-1]))
  
  # We have to use lapply over a different list depending on whether occurrence,
  # covariate or process has multiple modules

  if (length(process.module) > 1){
    process.output <- lapply(process.module, function(x) do.call(x[[1]], 
      c(list(occurrence = occurrence.output[[1]], ras = covariate.output[[1]]), x[-1])))
  } else if (length(covariate.module) > 1){
    process.output <- lapply(covariate.output, function(x) do.call(process[[1]], 
      c(list(occurrence = occurrence.output[[1]], ras = x), process.module[[1]][-1])))
  } else {
    process.output <- lapply(occurrence.output, function(x) do.call(process[[1]], 
      c(list(occurrence = x, ras = covariate.output[[1]]), process.module[[1]][-1])))
  }
  
  # Model module

  if (length(model.module) > 1){
    model.output <- lapply(model.module, function(x) do.call(x[[1]], 
       c(df = list(process.output[[1]]), x[-1])))
  } else {
    model.output <- lapply(process.output, function(x) do.call(model[[1]], 
       c(df = list(x), model.module[[1]][-1])))
  }

  #output module
  if (length(output.module) > 1){
    output.output <- lapply(output.module, function(x) do.call(x[[1]], 
       c(list(model.output[[1]], covariate.output[[1]]), x[-1])))
  } else if (length(covariate.module) > 1){
    output.output <- lapply(covariate.output, function(x) do.call(output[[1]], 
       c(list(model.output[[1]], x), output.module[[1]][-1])))    
  } else {
    output.output <- lapply(model.output, function(x) do.call(output[[1]], 
       c(list(x, covariate.output[[1]]), output.module[[1]][-1])))
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
#'
#'@return Name of the function. Adds function to global namespace.
#'@name GetModule


GetModule <- function(module){
  require(RCurl)
  zoonURL <- paste0('https://raw.githubusercontent.com/zoonproject/modules/master/R/', module, '.R')
  if (file.exists(module)){
    txt <- parse(text = paste(readLines(module), collapse="\n"))
  } else if (url.exists(zoonURL)){
    txt <- parse(text = getURL(zoonURL, ssl.verifypeer=FALSE))
  } else if (url.exists(module)){
    txt <- parse( text = getURL(module, ssl.verifypeer=FALSE))
  } else {
    stop('Cannot find the module. Check the URL or check that the module is at github.com/zoonproject')
  }
  # Probably do one environment up (i.e. in workflow environment) parent
  eval(txt, envir = parent.frame(1))
  eval(txt)
  new.func.name <- ls()[!ls() %in% c('module', 'txt', 'zoonURL')]
  return(new.func.name)
}

s



#'Turns named options into list ready to be used by workflow()
#'
#'@param module The module name or URL.
#'@param ... Any other parameters or options needed by that  module.
#'           All extra options must be named i.e. ModuleOptions('x', x=1)
#'           Not ModuleOptions('x', 1).
#'
#'
#'@return A list with all module options and the module name/URL in.
#'@name ModuleOptions
#'
#'@export
#'@examples print('No examples yet')

ModuleOptions <- function(module, ...){
  is.string(module)
  options <- list(module=module, ...)
  if ('' %in% names(options)){
    stop(paste0('Unnamed options in module ', module, ': All options must be named'))
  }
  
  return(list(module=module, ...))
}



# Helper to check if module is in structure of ModuleOptions() list
# And convert otherwise.

CheckModStructure <- function(x){
  if (is.string(x)){
      x <- ModuleOptions(x)
  }
  return(x)
}



# Helper to sort modules into lists.

CheckModList <- function(x){
  if (!is.list(x)){
    ModuleList <- list(CheckModStructure(x))
  } else if (identical(names(x[1]), 'module')){
    ModuleList <- list(x)
  } else {
    ModuleList <- lapply(x, CheckModStructure)
  }

  return(ModuleList)
}
  



