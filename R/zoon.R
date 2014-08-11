
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
#'@import assertthat


NULL


# ~~~~~~~~~~~~
# workflow wrapper function


#' workflow wrapper function
#'
#'@param extent A numeric vector of length 4 giving the coordinates of the 
#'      rectangular region within which to carry out the analysis, in the 
#'      order: xmin, xmax, ymin, ymax.
#'@param occurrence.module The name of the function (module) to be used to get occurence data
#'@param covariate.module  The name of the function (module) to be used to get covariate data
#'@param process.module The name of the function (module) to be used to process the data
#'@param model.module The name of the SDM model function (module) to be used 
#'@param map.module The name of the function (module) to be used to map output
#'
#'@return A list with the extent, the results of each module and a copy of the
#'       code used to execute the workflow (what's there now should be source-able
#'       though I'm sure there is a much neater approach than the one I took - the
#'       ultimate aim would be a much nicer way of enhancing reproducibility).
#'@export
#'@name workflow
#'@examples 
#'# define the extent in lat and long
#'uk.extent <- c(xmin = -10,
#'              xmax = 10,
#'              ymin = 45,
#'              ymax = 65)
#'
#'# run a workflow, using the logistic regression model
#'\dontrun{ans1 <- workflow(extent = uk.extent,
#'                 occurrence.module = 'AnophelesPlumbeus',
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

workflow <- function(extent,
                     occurrence.module,
                     covariate.module,
                     process.module,
                     model.module,
                     map.module) {
  
  # Get the modules (functions) from github. 
  # Save name of functions as well as load functions into global namespace.
  # Will probably want to make this so it checks namespace first.
  occurrence <- GetModule(occurrence.module)
  covariate <- GetModule(covariate.module)
  process <- GetModule(process.module)
  model <- GetModule(model.module)
  map.module <- GetModule(map.module)

  occurrence.output <- do.call(occurrence, list(extent))
  covariate.output <- do.call(covariate, list(extent))
  process.output <- do.call(process, list(occurrence.output, covariate.output))
  model.output <- do.call(model, list(process.output))
  map.output <- do.call(map.module, list(model.output, covariate.output))
 
  # get the command used to call this function
  bits <- sys.call()
  call <- paste0(bits[1],
                 '(', 
                 paste(bits[-1],
                       collapse = ', '),
                 ')')
  

  
  return(list(extent = extent,
              occurrence.output = occurrence.output,
              covariate.output = covariate.output,
              process.output = process.output,
              model.output = model.output,
              map.output = map.output))
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




#'Turn a function in the namespace into a module.
#'Will later add functions to upload module to figshare etc.
#'And add testing that the module name is unique.
#'
#'@param object A function that will be made into a module file.
#'@param dir The directory to put the module into (defaults to the
#'      working directory.
#'@param type A string that defines the type of module. Possible module types
#'      are occurence, covariate, process, model, diagnostic and output.
#'
#'@return NULL. Outputs a file
#'@name BuildModule
#'
#'@export
#'@examples # Define some module function
#' NewModule <- function(extent){ 
#'   covs <- as.data.frame(df[, 5:ncol(df)])
#'   names(covs) <- names(df)[5:ncol(df)]
#'   m <- glm(df$value ~ .,
#'         data = covs,
#'         family = binomial)
#'  
#'   return (m)
#' }
#' 
#' # Then build it into a module file.
#' BuildModule(NewModule, type = 'process', dir='~/Desktop')
#'
#'

BuildModule <- function(object, type, dir='.'){
  assert_that(is(object, 'function'))
  is.writeable(dir)
  type <- tolower(type)
  assert_that(type %in% c('occurrence', 'covariate', 'process', 'model', 'diagnostic', 'output'))

  obj <- deparse(substitute(object))
          
  write(paste0('# A zoon module\n #@', type), file = paste0(dir, '/', obj, '.R'))
  dump(c(obj), file = paste0(dir, '/', obj, '.R'), append=TRUE)
}
  

