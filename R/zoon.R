
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


#' This is the main function in zoon. A full SDM workflow can be called with this function. 
#'
#'@param occurrence.module The name of the function (module) to be used to get occurence data
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
#'\dontrun{ans1 <- workflow(occurrence.module = 'AnophelesPlumbeus',
#'                 covariate.module = 'AirNCEP',
#'                 process.module = 'OneHundredBackground',
#'                 model.module = 'LogisticRegression',
#'                 map.module = 'SameTimePlaceMap')
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

workflow <- function(occurrence.module,
                     covariate.module,
                     process.module,
                     model.module,
                     output.module) {
  
  # Check all modules are of same list structure
  occurrence.module <- CheckModStructure(occurrence.module)
  covariate.module <- CheckModStructure(covariate.module)
  process.module <- CheckModStructure(process.module)
  model.module <- CheckModStructure(model.module)
  output.module <- CheckModStructure(output.module)
  

  # Get the modules (functions) from github. 
  # Save name of functions as well as load functions into global namespace.
  # Will probably want to make this so it checks namespace first.
  occurrence <- GetModule(occurrence.module$module)
  covariate <- GetModule(covariate.module$module)
  process <- GetModule(process.module$module)
  # Check for val type lon lat covs
  model <- GetModule(model.module$module)
  # Test for predict method
  output.module <- GetModule(output.module$module)

  occurrence.output <- do.call(occurrence, occurrence.module[-1])
  covariate.output <- do.call(covariate, covariate.module[-1])
  process.output <- do.call(process, c(list(occurrence = occurrence.output, ras = covariate.output), process.module[-1]))
  model.output <- do.call(model, c(df = list(process.output), model.module[-1]))
  output <- do.call(output.module, c(list(model.output, covariate.output), output.module[-1]))
 
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
              output = output))
}


#'A function to get a module function.
#'It assumes the module is in github.com/zoonproject unless it is a full url
#'or a half url when it assumes the module is in another github repo.
#'Assigns the function to the global environment.
#'
#'@param module A string that describes the location of the R file. 
#'      Assumed to be in the zoonproject/modules repo unless name/repo/module.r 
#'      given.
#'
#'@return Name of the function. Adds function to global namespace.
#'@name GetModule


GetModule <- function(module){
  require(RCurl)
  zoonURL <- paste0('https://raw.githubusercontent.com/zoonproject/modules/master/R/', module, '.R')
  if (url.exists(zoonURL)){
    txt <- parse(text = getURL(zoonURL, ssl.verifypeer=FALSE))
  } else if (url.exists(module)){
    txt <- parse( text = getURL(module, ssl.verifypeer=FALSE))
  } else {
    stop('Cannot find the module. Check the URL or check that the module is at github.com/zoonproject')
  }
  eval(txt, envir = .GlobalEnv)
  eval(txt)
  new.func.name <- ls()[!ls() %in% c('module', 'txt', 'zoonURL')]
  return(new.func.name)
}







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
  if('' %in% names(options)){
    stop(paste0('Unnamed options in module ', module, ': All options must be named'))
  }
  
  return(list(module=module, ...))
}



# Helper to check if module is in structure of ModuleOptions() list
# And convert otherwise.

CheckModStructure <- function(x){
  if(is.string(x)){
      x <- ModuleOptions(x)
  }
  return(x)
}



