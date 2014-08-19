
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
          
  write(paste0('# A zoon module\n# @', type), file = paste0(dir, '/', obj, '.R'))
  dump(c(obj), file = paste0(dir, '/', obj, '.R'), append=TRUE)
}
  

