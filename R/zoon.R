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
#'@param ext A numeric vector of length 4 giving the coordinates of the 
#'      rectangular region within which to carry out the analysis, in the 
#'      order: xmin, xmax, ymin, ymax.
#'@param occurrenceFn The name of the function (module) to be used to get occurence data
#'@param covariateFn  The name of the function (module) to be used to get covariate data
#'@param processFn The name of the function (module) to be used to process the data
#'@param modelFn The name of the SDM model function (module) to be used 
#'@param mapFn The name of the function (module) to be used to map output
#'
#'@return A list with the extent, the results of each module and a copy of the
#'       code used to execute the workflow (what's there now should be source-able
#'       though I'm sure there is a much neater approach than the one I took - the
#'       ultimate aim would be a much nicer way of enhancing reproducibility).
#'@export
#'@name workflow
#'@examples x <- 2+2

workflow <- function(ext,
                     occurrenceModule,
                     covariateModule,
                     processModule,
                     modelModule,
                     mapModule) {
  
  # Get the modules (functions) from github. 
  # Save name of functions as well as load functions into global namespace.
  # Will probably want to make this so it checks namespace first.
  occurrence <- getModule(occurrenceModule)
  covariate <- getModule(covariateModule)
  process <- getModule(processModule)
  model <- getModule(modelModule)
  mapMod <- getModule(mapModule)

  occ <- do.call(occurrence, list(ext))
  cov <- do.call(covariate, list(ext))
  df <- do.call(process, list(occ, cov))
  m <- do.call(model, list(df))
  map <- do.call(mapMod, list(m, cov))
 
  # get the command used to call this function
  bits <- sys.call()
  call <- paste0(bits[1],
                 '(', 
                 paste(bits[-1],
                       collapse = ', '),
                 ')')
  
  # sorry about this spaghetti...
  workflow <- paste0(paste(bits[-2], 
                           c(getSource(workflow),
                             getSource(eval(parse(text = occurrence))),
                             getSource(eval(parse(text = covariate))),
                             getSource(eval(parse(text = process))),
                             getSource(eval(parse(text = model))),
                             getSource(eval(parse(text = mapMod)))),
                           sep = ' <- ',
                           collapse = '\n\n'),
                     paste0('\n\n',
                            bits[2],
                            ' <- c(',
                            paste(ext, collapse = ', '),
                            ')\n'),
                     '\nans <- ',
                     call,
                     collapse = '\n\n\n')
  
  return(list(extent = extent,
              occ = occ,
              cov = cov,
              df = df,
              m = m,
              map = map,
              workflow = workflow))
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
#'@name getModule


getModule <- function(module){
  require(RCurl)
  zoonURL <- paste0('https://raw.githubusercontent.com/zoonproject/modules/master/R/',module,'.R')
  if(url.exists(zoonURL)){
    txt <- parse(text = getURL(zoonURL, ssl.verifypeer=FALSE))
  } else if(url.exists(module)){
    txt <- parse( text = getURL(module, ssl.verifypeer=FALSE))
  } else{
    stop('Cannot find the module. Check the URL or check that the module is at github.com/zoonproject')
  }
  eval(txt, envir = .GlobalEnv)
  eval(txt)
  newFunc <- ls()[!ls() %in% c('module', 'txt', 'zoonURL')]
  return(newFunc)
}


#'small helper function to dump the code for a function to a text string
#'
#'@param object A function whose source code will be outputted as a string.
#'
#'@return Text string of the function
#'@name getSource
#'

getSource <- function (object) {
  paste(deparse(object), collapse = '\n')
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
#'@name buildModule
#'
#'@export
#'@examples # Define some module function
#' newModule <- function(ext){ 
#' covs <- as.data.frame(df[, 5:ncol(df)])
#' names(covs) <- names(df)[5:ncol(df)]
#' m <- glm(df$value ~ .,
#'         data = covs,
#'         family = binomial)
#'  
#' return (m)
#' }
#' 
#' # Then build it into a module file.
#' buildModule(newModule, type = 'process', dir='~/Desktop')
#'
#'

buildModule <- function (object, type, dir='.') {
  assert_that(is(object, 'function'))
  is.writeable(dir)
  type <- tolower(type)
  assert_that(type %in% c('occurrence', 'covariate', 'process', 'model', 'diagnostic', 'output'))

  obj <- deparse(substitute(object))
          
  write(paste0('# A zoon module\n #@', type), file = paste0(dir, '/',obj, '.R'))
  dump(c(obj), file = paste0(dir, '/',obj, '.R'), append=TRUE)
}
  

