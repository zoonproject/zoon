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
                     occurrenceFn,
                     covariateFn,
                     processFn,
                     modelFn,
                     mapFn) {
  

  occ <- occurrenceFn(ext)
  cov <- covariateFn(ext)
  df <- processFn(occ, cov)
  m <- modelFn(df)
  map <- mapFn(m, cov)
  
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
                             getSource(occurrenceFn),
                             getSource(covariateFn),
                             getSource(processFn),
                             getSource(modelFn),
                             getSource(mapFn)),
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
#'
#'@param module A string that describes the location of the R file. 
#'      Assumed to be in the zoonproject/modules repo unless name/repo/module.r 
#'      given.
#'
#'@return Nothing. Adds function to namespace.
#'@name getModule
#'@export


getModule <- function(module){
        require(RCurl)
        if(url.exists(paste0('https://raw.githubusercontent.com/zoonproject/modules/master/R/',module,'.R'))){
                txt <- getURL(
                        paste0('https://raw.githubusercontent.com/zoonproject/modules/master/R/',module,'.R'), 
                        ssl.verifypeer=FALSE)
                changedFuncName <- txt
                newFunction <- parse( text = txt )
        } else if(url.exists(module)){
                newFunction <- parse( text = getURL(module, ssl.verifypeer=FALSE) )
        }
        eval(newFunction)
}

#'small helper function to dump the code for a function to a text string
#'
#'@param object A function.
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
#'@examples

buildModule <- function (object, dir='.', type) {
        assert_that(is(object, 'function'))
        is.writeable(dir)
        assert_that(type %in% c('occurrence', 'covariate', 'process', 'model', 'diagnostic', 'output'))

        obj <- deparse(substitute(object))
                
        write(paste0('# A zoon module\n #@', type), file = paste0(dir, '/',obj, '.R'))
        dump(c(obj), file = paste0(dir, '/',obj, '.R'), append=TRUE)
}
  

