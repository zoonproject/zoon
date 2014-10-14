#'BuildModule
#'
#'Turn a function in the namespace into a module.
#'Will later add functions to upload module to figshare etc.
#'And add testing that the module name is unique.
#'
#'@param object A function that will be made into a module file.
#'@param dir The directory to put the module into (defaults to the
#'      working directory.
#'@param type A string that defines the type of module. Possible module types
#'      are occurence, covariate, process, model, diagnostic and output.
#'@param title A short description of the module.
#'@param description A single string giving a full description of the model.
#'@param paras A list of the form 
#'  list(parameterName = 'Parameter description.',
#'    anotherParameter = 'Another descriptions.')
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

BuildModule <- function(object, type, dir='.', title = '',  description = '', author = '', email = '', paras=NULL){
  assert_that(is(object, 'function'))
  is.writeable(dir)
  
  type <- tolower(type)
  assert_that(type %in% c('occurrence', 'covariate', 'process', 'model', 'diagnostic', 'output'))

  obj <- deparse(substitute(object))
  
  if(is.null(paras)){
    paras <- formals(object)
  }

  paraNames <- names(paras)
  paraDocs <- paste(sapply(paraNames, function(x) paste("#'@param", x, paras[x], "\n")), collapse="#'\n")

  # Roxygen2 uses @ as a tag. So have to double it.
  email <- gsub('@', '@@', email)
        
  docs <- paste0("#'", obj, ": ", title, "\n#'\n#'", description, "\n#'\n#'", 
            "Module type: ", toupper(substring(type, 1,1)), substring(type, 2), 
            "\n#'\n", paraDocs, "#'\n#'@author ", author, 
            "'\n#'@author ", email, "'\n#'@name ", obj)

  write(docs, file = paste0(dir, '/', obj, '.R'))
  dump(c(obj), file = paste0(dir, '/', obj, '.R'), append=TRUE)
}
  

