
#'A function to load a module function from url or disk.
#'
#'Loads a module function into the global environment ready to be used in a 
#'  zoon workflow. This function is mostly for use while developing modules.
#'  Workflows run with modules defined locally are no longer reproducible and
#'  so are discouraged and will be tagged as 'unreproducible'. 
#'
#'@param module A string that describes the location of the R file. Can be a
#'  a full URL or a path to a local file.
#'
#'@return Name of the function. Adds function to global namespace.
#'@name LoadModule
#'@export

LoadModule <- function(module){
  # Sub and deparse argument, then remove extra quote marks.
  module <- deparse(substitute(module))
  module <- gsub('"', '', module)

  # Create url that matches zoon repo
  # .sha will vary based on whether this is pegged to a specific version of
  # modules
  zoonURL <- 
    sprintf('https://raw.githubusercontent.com/zoonproject/modules/%s/R/%s.R',
            .sha, module)

  # If module is a path, load module
  if (file.exists(module)){
    txt <- parse(text = paste(readLines(module), collapse="\n"))
  # If zoonURL is a zoon repo url, load module
  #   Could probably do same thing as GetModule here to avoid repeated web call
  } else if (url.exists(zoonURL, .opts=list(ssl.verifypeer=FALSE))){
    txt <- parse(text = getURL(zoonURL, ssl.verifypeer=FALSE))
  # If module on its own is a url, load module
  } else if (url.exists(module, .opts=list(ssl.verifypeer=FALSE))){
    txt <- parse( text = getURL(module, ssl.verifypeer=FALSE))
  # Otherwise throw error.
  } else {
    stop(paste('Cannot find "', module, 
      '". Check the URL or check that the module is at github.com/zoonproject'))
  }
  # Load to global environment
  eval(txt, envir = globalenv())
  # Return the actual name of the module that has been loaded.
  #   Don't just return 'module' argument as that can be url/path
  #   which isn't useful.
  # Cruddy code. But module name is the only other object in this
  #   call environment.
  eval(txt)
  new.func.name <- ls()[!ls() %in% c('module', 'txt', 'zoonURL')]
  return(new.func.name)
}



#'A function to get a module function.
#'
#'Checks for the module in the global namespace, then the zoon repo. Then reads
#'  the module source and loads the function.
#'
#'@param module A string that describes the location of the R file. Can be a
#'      module name assuming the module is in global namespace or 
#'      github.com/zoonproject/modules. Otherwise can be a full URL or a local
#'      file.
#'@param forceReproducible Do we want to force the function to get modules from
#'      the zoon repo, even if they exist in the global namespace.
#'
#'@return Name of the function. Function is run with workflow and new module
#'  function is added to workflow environment.
#'@name GetModule

GetModule <- function(module, forceReproducible){
  zoonURL <- 
    paste0('https://raw.githubusercontent.com/zoonproject/modules/master/R/',
           module, '.R')

  # If the module is in global namespace, use that function
  #   unless forceReproduce is TRUE, in which case we want to get from repo.
  #   
  # Get module from zoonURL otherwise.
  if (exists(module) & !forceReproducible){
    assign(module, eval(parse(text = module)),  envir = parent.frame(4))
    return(module)
  } else {
    rawText <- getURL(zoonURL, ssl.verifypeer=FALSE)
  } 

  # getURL returns "Not Found" if no webpage found.
  #   Use this to avoid two web call.s
  if(rawText == "Not Found") {
    stop(paste('Cannot find "', module, 
      '". Check that the module is on the zoon repository or in the global namespace.'))
  }

  # Parse text from webpage.
  txt <- parse(text = rawText)
  
  # Evaluate text in the workflow call environment
  eval(txt, envir = parent.frame(4))

  return(module)
}


#' A function to apply GetModule to a list correctly.
#'@param modules A list from CheckModList() given details for 
#'  one or more modules.
#'@param forceReproducible Logical to determine whether the modules should be 
#'  taken from repo even if they exist locally to enforce reproducibility.
#'@name LapplyGetModule

LapplyGetModule <- function(modules, forceReproducible){
  lapply(modules, function(x) 
    list.append(func = GetModule(as.character(x$module), forceReproducible), x))
}


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
#'@param modelFunction String giving the name of the model function which is 
#'  in turn the
#'  name of the module.
#'
#'@param paras All other parameters that should be passed to the model function. 
#'  i.e. model[[1]]$paras
#'
#'@param workEnv The environment name of the workflow call environment.
#'
#'@return A list of length 2 containing the model trained on all data and a
#'  data.frame which contains value, type, fold, lon, lat, predictions and 
#'  then all environmental variables.
#'
#'@name RunModels

RunModels <- function(df, modelFunction, paras, workEnv){
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
      modelFold <- do.call(modelFunction, c(df = list(df[df$fold != i, ]), 
                                            paras),
                           envir = workEnv)
      dfOut$predictions[df$fold == i] <- 
        predict(modelFold, newdata = df[df$fold == i, 6:NCOL(df), drop=FALSE], 
                type = 'response') 
    }
  }
  
  # Run model on all data except external validation data
  m <- do.call(modelFunction, c(df = list(df[df$fold != 0, ]), paras), 
               envir = workEnv)
  
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




#' CheckModList
#'
#' Helper to sort module arguments into lists with common structure.
#'   Want any input to end up as 
#'   list(module = moduleName, paras = list(paraName1 = para, paraName2 = 2)).
#'   See tests/testthat/testZoon.R for list of potential input.
#'@param x A 'call' or 'character' from substitute(occurrence) etc. So either
#'    quoted which yields a character substitute('module1') or unquotes which
#'    yields a call substitute(module1). 
#'@name CheckModList

# Check passing as quoted. e.g. occurrence = "ModuleName(k=2)"
# Also occurrence = "list(mod1, mod2)" is probably bad.

CheckModList <- function(x){

  # Should accept occurrence = 'module1', but NOT 
  #   occurrence = 'module1(k=2)', or occurrence = 'list(mod1, mod1)'
  if (inherits(x, 'character')){
    if (grepl('[!#$%&*+,-/:;<>?@[\ ]^_`| ]', x)){
      stop(paste('If specifying module arguments please use the form',
        'Module(para = 2), without quotes. No special characters should exist',
        'in module names.'))
    }
  }
  
  # If argument is passed as unquoted moduleName: occurrence = ModuleName, 
  if (class(x) == 'name'){
    ModuleList <- list(list(module = as.character(x), paras = list()))
  
  # If list of modules given: occurrence = list(Mod1, Mod2), 
  #   If list(Mod1(k=2), Mod2(p = 3)), parameters sorted in 
  #   FormatModuleList
  } else if (x[[1]] == 'list') {
    listCall <- as.list(x)
    listCall[[1]] <- NULL
    ModuleList <- lapply(listCall, FormatModuleList)

  # If Chained modules given: occurrence = Chain(Mod1, Mod2),
  } else if (x[[1]] == 'Chain'){
    listCall <- as.list(x)
    listCall[[1]] <- NULL
    ModuleList <- lapply(listCall, FormatModuleList) 
    attr(ModuleList, 'chain') <- TRUE
  
  # If unquoted module w/ paras given: occurrence = Module1(k=2)
  } else if (identical(class(x[[1]]), 'name')){
    # Parameters
    paras <- as.list(x)
    paras[[1]] <- NULL
    ModuleList <- list(list(module = as.character(x[[1]]), paras = paras))
  # Deal with all quoted forms
  #   Can include 'module1', 'module(para = 2)', 'module(p = 2, q = 'wer')'
  } else if(inherits(x, 'character')){
    ModuleList <- list(SplitArgs(x))
  }  else {
    stop(paste('Please check the format of argument', as.character(x)))
  } 

  return(ModuleList)
}



#' Split a string into a module and it's arguments
#'
#' A function that takes a string (from workflow$call) and splits it into a
#'   module name and it's arguments.
#'
#'@param string A string of the form "moduleName" or 
#'  "moduleName(parameter = 2, parameter2 = 3)"
#'
#'@name SplitArgs

SplitArgs <- function(string){
  module <- gsub('(^)(.*)(\\(.*$)', '\\2', string)
  if(grepl('\\(', string)){
    args <- gsub('(^.*\\()(.*)(\\)$)', '\\2', string)
  } else {
    args <- ''
  }
  sepArgs <- (strsplit(args, ','))[[1]]
  arguments <- lapply(strsplit(sepArgs, '='), 
    function(x) gsub(' ', '', x[2]))
  names(arguments) <- unlist(lapply(strsplit(sepArgs, '='), 
    function(x) gsub(' ', '', x[1])))
  return(list(module = module, paras = arguments))
}

#'FormatModuleList
#'
#' Little helper to format module lists. Want to return a list
#'   newList$module is the module name and newList$paras is a list
#'   of the parameters given for the module.
#'@param x An object of class 'name' or 'call' e.g. module1 or module1(k=2).
#'@name FormatModuleList

FormatModuleList <- function(x){
  # Turn 'call' or 'name' into list.
  listx <- as.list(x)

  # Empty list to populate.
  newList <- list()
  newList$module <- listx[[1]]
  # Remove list element which contains modules name. Remaining is parameters.
  listx[[1]] <- NULL
  newList$paras <- listx
  return(newList)
}



#' ExtractAndCombData
#'
#' Simply extract covariates from rasters and combine with occurrence data.
#'
#'@param occurrence A data frame from an occurrence module
#'@param ras Environmental raster layer, stack or brick.
ExtractAndCombData <- function(occurrence, ras){
  
  # Check that all points are within the raster
  bad.coords <- is.na(cellFromXY(ras,
                                 occurrence[,c('longitude', 'latitude')]))
  if(any(bad.coords)){
    occurrence <- occurrence[!bad.coords, ]
    warning ('Some occurrence points are outside the raster extent and have been removed before modelling')
  }

  # extract covariates from lat long values in df.
  occurrenceCovariates <- as.matrix(raster::extract(ras, occurrence[, c('longitude', 'latitude')]))
  names(occurrenceCovariates) <- names(ras)  

  # combine with the occurrence data
  df <- cbind(occurrence, occurrenceCovariates)

  
  # Return as list of df and ras as required by process modules
  return(list(df=df, ras=ras))
  
}


#'Chain
#'
#'This function does nothing. However using Chain(modules... ) in a call
#'  to workflow will chain the modules together rather than  
#'  run in separate analyses. For occurrence or covariate modules the datasets are joined. 
#'  Processes are run sequentially.
#'@param ... List of modules to be chained.
#'
#'@name Chain

Chain <- function(...){
  NULL
}


#'SortArgs
#'
#'
#' Helper to take substituted args from workflow call and paste them into 
#'   a runeable workflow function.
#'@name SortArgs

SortArgs <- function(occSub, covSub, proSub, modSub, outSub, forceReproducible){
  call <- paste0("workflow(", 
                 "occurrence = ", occSub,
               ", covariate = ", covSub,
               ", process = ", proSub,
               ", model = ", modSub,
               ", output = ", outSub,
               ", forceReproducible = ", as.character(forceReproducible), ")")
}    



#'SplitCall
#'
#' Helper to split a character string workflow call, as inherited from zoonWorkflow
#'   into it's constituent arguments
#'@param call A character string of a valid zoon workflow call.
#'@name SplitCall

SplitCall <- function(call){

  # Regex to find each argument within call.
  #   Find 3 patterns and sub whole string with just middle pattern
  #   Middle pattern is the argument name.
  occurrence <- gsub('(.*occurrence = )(.*)(, covariate.*$)', '\\2', call)
  covariate <- gsub('(.*covariate = )(.*)(, process.*$)', '\\2', call)
  process <- gsub('(.*process = )(.*)(, model.*$)', '\\2', call)
  model <- gsub('(.*model = )(.*)(, output.*$)', '\\2', call)
  output <- gsub('(.*output = )(.*)(, forceReproducible.*$)', '\\2', call)
  forceReproducible <- gsub('(.*forceReproducible = )(.*)())', '\\2', call)

  # Make vector and add names.
  split <- c(occurrence, covariate, process, model, output, forceReproducible)
  names(split) <- c('occurrence', 'covariate', 'process', 
    'model', 'output', 'forceReproducible')
  
  return(split)

}


#'ErrorAndSave
#' Function used in tryCatch calls in workflow.
#'   If an error is caught, save the workflow to tmpZoonWorkflow.
#'   And return some useful messages. Then stop().
#'   cond is the error messages passed by try catch.
#'   mod is the modules number (1:5) to give NULLS to the correct modules.
#'
#'@param e The workflow call environment
#'@param cond The error message that was caught in tryCatch.
#'@param mod Which module has failed? 1=occurrence, 2=covariate, 3=process
#'  4=model, 5=output.
#'@name ErrorAndSave

ErrorAndSave <- function(cond, mod, e){

  # Create list to be populated
  #  Include the call from the workflow environment e
  w <- list(occurrence.output = NULL,
       covariate.output = NULL,
       process.output = NULL,
       model.output = NULL,
       report = NULL,
       call = e$call)
  
  # Depending on mod argument, replace NULLS in w with the value of module
  #   output. To get the module output we have to reference the workflow
  #   environment which was given as argument e.
  if(mod > 1){
    w$occurrence.output <- e$occurrence.output
  }
  if(mod > 2){
    w$covariate.output <- e$covariate.output
  }
  if(mod > 3){
    w$process.output <- e$process.output
  }  
  if(mod > 4){
    w$model.output <- e$model.output
  }
  class(w) <- 'zoonWorkflow'

  # Select the module type using numeric mod argument
  module <- c('occurrence', 'covariate', 'process', 'model', 'output')[mod]

  # R CMD check apparently dislikes this assignment to the global environemtn
  assign('tmpZoonWorkflow', w,  envir = .GlobalEnv)

  # Give useful messages.
  # What were the errors that were caught be tryCatch.
  message('Caught errors:\n',  cond)
  message()
  # Where did workflow break and where is the progress stored?
  x <- paste("Stopping workflow due to error in", module, "module.\n", 
             "Workflow progress stored in object 'tmpZoonWorkflow'.")
  # Throw error. The call for this error is meaningless so don't print it.
  stop(x, call. = FALSE)
}


#' PasteAndDep
#'
#' Paste and deparse. Helper to format substituted args. If arguments were 
#'   chained or listed then substitute gives a list. We want to paste it back together.
#'@name PasteAndDep

PasteAndDep <- function(x){
  paste(deparse(x), collapse = ' ')
}



