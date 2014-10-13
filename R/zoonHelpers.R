
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
  zoonURL <- 
    paste0('https://raw.githubusercontent.com/zoonproject/modules/master/R/',
           module, '.R')
  if (file.exists(module)){
    txt <- parse(text = paste(readLines(module), collapse="\n"))
  } else if (url.exists(zoonURL, .opts=list(ssl.verifypeer=FALSE))){
    txt <- parse(text = getURL(zoonURL, ssl.verifypeer=FALSE))
  } else if (url.exists(module, .opts=list(ssl.verifypeer=FALSE))){
    txt <- parse( text = getURL(module, ssl.verifypeer=FALSE))
  } else {
    stop(paste('Cannot find "', module, 
      '". Check the URL or check that the module is at github.com/zoonproject'))
  }
  # Load to global environment
  eval(txt, envir = globalenv())
  eval(txt)
  new.func.name <- ls()[!ls() %in% c('module', 'txt', 'zoonURL')]
  return(new.func.name)
}

#'A function to get a module function.
#'
#'Checks for the module in the global namespace, then the zoon repo, then 
#'
#'@param module A string that describes the location of the R file. Can be a
#'      module name assuming the module is in global namespace or github.com/zoonproject/modules.
#'      Otherwise can be a full URL or a local file.
#'
#'@return Name of the function. Function is run with workflow and new module
#'  function is added to workflow environment.
#'@name GetModule

GetModule <- function(module){
  zoonURL <- 
    paste0('https://raw.githubusercontent.com/zoonproject/modules/master/R/',
           module, '.R')
  if (exists(module)){
    assign(module, eval(parse(text = module)),  envir = parent.frame(4))
    return(module)
  } else if (url.exists(zoonURL, .opts=list(ssl.verifypeer=FALSE))){
    txt <- parse(text = getURL(zoonURL, ssl.verifypeer=FALSE))
  } else {
    stop(paste('Cannot find "', module, 
      '". Check that the module is on the zoon repository or in the global namespace.'))
  }
  # Probably do one environment up (i.e. in workflow environment)
  eval(txt, envir = parent.frame(4))
  eval(txt)
  new.func.name <- ls()[!ls() %in% c('module', 'txt', 'zoonURL')]
  return(new.func.name)
}


#' A function to apply GetModule to a list correctly.
#'@param modules A list from CheckModList() given details for 
#'  one or more modules.
#'@name GetModules

GetModules <- function(modules){
  return(lapply(modules, function(x) list.append(func = GetModule(as.character(x$module)), x)))
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
  
  # Run model on all data.
  m <- do.call(modelFunction, c(df = list(df), paras), 
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






# Helper to sort modules into lists.

CheckModList <- function(x){
  if (class(x) == 'name'){
    ModuleList <- list(list(module = as.character(x), paras = list()))
  } else if (x[[1]] == 'list') {
    listCall <- as.list(x)
    listCall[[1]] <- NULL
    ModuleList <- lapply(listCall, FormatModuleList)
  } else if (x[[1]] == 'Chain'){
    listCall <- as.list(x)
    listCall[[1]] <- NULL
    ModuleList <- lapply(listCall, FormatModuleList) 
    attr(ModuleList, 'chain') <- TRUE
  } else {
    paras <- as.list(x)
    paras[[1]] <- NULL
    ModuleList <- list(list(module = as.character(x[[1]]), paras = paras))
  }
  
  return(ModuleList)
}

# Little helper to format module lists
FormatModuleList <- function(x){
  listx <- as.list(x)
  newList <- list()
  newList$module <- listx[[1]]
  listx[[1]] <- NULL
  newList$paras <- listx
  return(newList)
}




# Simply extract covariates from rasters and combine with occurrence.
ExtractAndCombData <- function(occurrence, ras){

  noccurrence <- nrow(occurrence)
  
  # extract covariates
  occ_covs <- as.matrix(raster::extract(ras, occurrence[, c('longitude', 'latitude')]))
  
  # combine with the occurrence data
  df <- cbind(occurrence,
                   occ_covs)
  
  names(df)[6:ncol(df)] <- names(ras)
  
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
#'@export



Chain <- function(...){
  NULL
}


# Helper to take substituted args from workflow call and paste them into 
# a runeable workflow function.

sortArgs <- function(occSub, covSub, proSub, modSub, outSub){
  call <- paste0("workflow(", 
                 "occurrence = ", occSub,
               ", covariate = ", covSub,
               ", process = ", proSub,
               ", model = ", modSub,
               ", output = ", outSub, ")")
}    



# Helper to split a character string of a workflow call, as inherited from zoonWorkflow
#   into it's constituent modules

splitCall <- function(call){
  occurrence <- gsub('(.*occurrence = )(.*)(, covariate.*$)', '\\2', call)
  covariate <- gsub('(.*covariate = )(.*)(, process.*$)', '\\2', call)
  process <- gsub('(.*process = )(.*)(, model.*$)', '\\2', call)
  model <- gsub('(.*model = )(.*)(, output.*$)', '\\2', call)
  output <- gsub('(.*output = )(.*)())', '\\2', call)

  split <- c(occurrence, covariate, process, model, output)
  names(split) <- c('occurrence', 'covariate', 'process', 'model', 'output')
  
  return(split)

}


# Function used in tryCatch calls in workflow.
# If an error is caught, save the workflow to tmpZoonWorkflow.
# And return some useful messages. Then stop().
# cond is the error messages passed by try catch.
# mod is the modules number (1:5) to give NULLS to the correct modules.

ErrorAndSave <- function(cond, mod = 1, e){

  w <- list(occurrence.output = NULL,
       covariate.output = NULL,
       process.output = NULL,
       model.output = NULL,
       report = NULL,
       call = e$call)
  
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

  module <- c('occurrence', 'covariate', 'process', 'model', 'output')[mod]

  assign('tmpZoonWorkflow', w,  envir = .GlobalEnv)

  message('Caught errors:\n',  cond)
  message()
  x <- paste("Stopping workflow due to error in", module, "module.\n", 
             "Workflow progress stored in object 'tmpZoonWorkflow'.")
  stop(x, call. = FALSE)
}



# Helper to format substituted args ie modules

PasteAndDep <- function(x){
  paste(deparse(x), collapse = ' ')
}



