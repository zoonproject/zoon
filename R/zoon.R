
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
#'@import assertthat raster rlist RCurl httr httpuv


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
  occurrence <- GetModules(occurrence.module) 
  covariate <- GetModules(covariate.module) 
  process <- GetModules(process.module) 
  # Check for val type lon lat covs
  model <- GetModules(model.module) 
  # Test for predict method
  output <- GetModules(output.module) 


  # stack(lapply(covariate.module, function(x) do.call(x[[1]], x[-1]))) Not needed
  # now but is basis for CovarDaisyChain()

  # Run the modules.
  # First the data collection modules
  occurrence.output <- lapply(occurrence, function(x) do.call(x$func, x$paras))
  covariate.output <- lapply(covariate, function(x) do.call(x$func, x$paras))
  
  # We have to use lapply over a different list depending on whether occurrence,
  # covariate or process has multiple modules

  if (length(process) > 1){
    process.output <- lapply(process, function(x) do.call(x$func, 
      c(list(occurrence = occurrence.output[[1]], ras = covariate.output[[1]]), x$paras)))
  } else if (length(covariate.module) > 1){
    process.output <- lapply(covariate.output, function(x) do.call(process[[1]]$func, 
      c(list(occurrence = occurrence.output[[1]], ras = x), process[[1]]$paras)))
  } else {
    process.output <- lapply(occurrence.output, function(x) do.call(process[[1]]$func, 
      c(list(occurrence = x, ras = covariate.output[[1]]), process[[1]]$paras)))
  }
  
  # Model module

  if (length(model.module) > 1){
    model.output <- lapply(model, function(x) do.call(x$func, 
       c(df = list(process.output[[1]]), x$paras)))
  } else {
    model.output <- lapply(process.output, function(x) do.call(model[[1]]$func, 
       c(df = list(x), model[[1]]$paras)))
  }

  #output module
  if (length(output.module) > 1){
    output.output <- lapply(output, function(x) do.call(x$func, 
       c(list(model.output[[1]], covariate.output[[1]]), x$paras)))
  } else if (length(covariate.module) > 1){
    output.output <- lapply(covariate.output, function(x) do.call(output[[1]]$func, 
       c(list(model.output[[1]], x), output[[1]]$paras)))    
  } else {
    output.output <- lapply(model.output, function(x) do.call(output[[1]]$func, 
       c(list(x,  covariate.output[[1]]), output[[1]]$paras)))
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


GetModule <- function(module, type=''){
  zoonURL <- paste0('https://raw.githubusercontent.com/zoonproject/modules/master/R/', module, '.R')
  if (file.exists(module)){
    txt <- parse(text = paste(readLines(module), collapse="\n"))
  } else if (url.exists(zoonURL, .opts=list(ssl.verifypeer=FALSE))){
    txt <- parse(text = getURL(zoonURL, ssl.verifypeer=FALSE))
  } else if (url.exists(module, .opts=list(ssl.verifypeer=FALSE))){
    txt <- parse( text = getURL(module, ssl.verifypeer=FALSE))
  } else {
    stop('Cannot find the module. Check the URL or check that the module is at github.com/zoonproject')
  }
  # Probably do one environment up (i.e. in workflow environment) parent
  eval(txt, envir = parent.frame(4))
  #eval(txt, envir = globalenv())
  eval(txt)
  new.func.name <- ls()[!ls() %in% c('module', 'txt', 'zoonURL', 'type')]
  return(new.func.name)
}


#' A function to apply GetModule to a list correctly.
#'@param modules A list from CheckModList() given details for 
#'  one or more modules.
#'@param type String describing the type of module. Only needed if getting from
#'  zoon github.
#'@name GetModules

GetModules <- function(modules, type=''){
  return(lapply(modules, function(x) list.append(func = GetModule(x$module, type), x)))
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
  



