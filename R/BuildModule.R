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
#'      are occurrence, covariate, process, model, diagnostic and output.
#'@param title A short description of the module.
#'@param description A single string giving a full description of the model.
#'@param paras A list of the form 
#'  list(parameterName = 'Parameter description.',
#'    anotherParameter = 'Another descriptions.')
#'@param author String giving the author(s) name(s)
#'@param email Stirng giving the correspondance address for the module.
#'
#'@return NULL. Outputs a file
#'@name BuildModule
#'
#'@export
#'

BuildModule <- function(object, type, dir='.', title = '',  description = '', author = '', email = '', paras=NULL){
  assert_that(is(object, 'function'))
  assert_that(tolower(type) %in% c('occurrence', 'covariate', 'process', 'model', 'diagnostic', 'output'))
  is.writeable(dir)

  
  
  # Is all meta information provided.
  if(title == '' | description == '' | author == '' | email == '') {
    complete <- FALSE
  } else {
    complete <- TRUE
  }

  # What are the default arguments for each module type
  defArgs <- list(occurrence = NULL, covariate = NULL, process = c('data'),
                    model = c('df'), output = c('model', 'ras'))


  missingParas <- names(formals(object))[!names(formals(object)) %in% names(paras)]

  # Are all parameters documented (excluding defualt, zoon internal parameters).
  #   If not give a warning.
  if(any(!missingParas %in% defArgs[[type]])){
    complete <- FALSE
  }
  if(!complete){
    warning(paste('Information not complete. All arguments must be filled and',
      'all parameters documented before uploading module to Zoon repository.'))
  }


  # Test that model has correct inputs.
  if(any(!defArgs[[type]] %in% names(formals(object)))){
    stop(paste0(type, " modules must contain arguments '", 
      paste(defArgs[[type]], collapse = "' and '"), "'."))
  }
    

  type <- tolower(type)
  obj <- deparse(substitute(object))
  

  # Sort out parameter formating.
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
  

