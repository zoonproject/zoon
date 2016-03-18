
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
  
  # module must be a string 
  
  
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
               '". Check the file path or URL exist'))
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



#A function to get a module function.
#
#Checks for the module in the global namespace, then the zoon repo. Then reads
#  the module source and loads the function.
#
#@param module A string that describes the location of the R file. Can be a
#      module name assuming the module is in global namespace or 
#      github.com/zoonproject/modules. Otherwise can be a full URL or a local
#      file.
#@param forceReproducible Do we want to force the function to get modules from
#      the zoon repo, even if they exist in the global namespace.
#
#@return Name of the function. Function is run with workflow and new module
#  function is added to workflow environment.
#@name GetModule

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
    attr(module, 'version') <- 'local copy'
    return(module)
  } else {
    rawText <- getURL(zoonURL, ssl.verifypeer = FALSE)
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
  
  # Assign version attribute
  attr(module, 'version') <- GetModuleVersion(rawText)
  
  return(module)
}


# A function to apply GetModule to a list correctly.
#@param modules A list from CheckModList() given details for 
#  one or more modules.
#@param forceReproducible Logical to determine whether the modules should be 
#  taken from repo even if they exist locally to enforce reproducibility.
#@name LapplyGetModule

LapplyGetModule <- function(modules, forceReproducible){
  lapply(modules, function(x){ 
    GotModule <- GetModule(as.character(x$module), forceReproducible)
    return(c(x, func = GotModule, version = attr(GotModule, 'version')))
  }
  )
}


#'RunModels
#'
#' A function to train and predict crossvalidation folds
#' and train one full model and predict any external validation data.
#' This function is primarily used internally but can be used when
#' running workflows interactively (see vignette \code{basic zoon useage})
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
#'@export
#'@name RunModels

RunModels <- function(data, modelFunction, paras, workEnv){
  # Count non zero folds
  # 0 are for external validation only. 
  k <- length(unique(data$df$fold)[unique(data$df$fold) != 0])
  
  # Init. output dataframe with predictions column
  dfOut <- cbind(data$df, predictions = NA)
  # Not necessary?
#  names(dfOut)[7:ncol(dfOut)] <- names(df)[6:ncol(df)]
  
  # We don't know that they want cross validation.
  # If they do, k>1, then run model k times and predict out of bag
  # Skip otherwise
  if(k > 1){
    for(i in 1:k){
      # Get rows within current fold
      idx <- which(data$df$fold != i)
      modelFold <- do.call(modelFunction, c(.df = list(data$df[idx, ]),
                                            .obs.covariates = list(data$obs.covariates[idx, ]),
                                            .site.covariates = list(data$site.covariates[idx, ]),
                                            paras), envir = workEnv)
      ## Merge observation and site level covariates
      pred <- ZoonPredict(modelFold,
                          newdata = list(site.covariates = data$site.covariates[idx, , drop = FALSE],
                                         obs.covariates = data$obs.covariates[idx, , drop = FALSE]))
      
      dfOut$predictions[data$df$fold == i] <- pred 
      
    }
  }
  
  # Run model on all data except external validation data
  m <- do.call(modelFunction,  c(.df = list(data$df[data$df$fold != 0, ]),
                                 .obs.covariates = list(data$obs.covariates[data$df$fold != 0, ]),
                                 .site.covariates = list(data$site.covariates[data$df$fold != 0, ]),
                                 paras), envir = workEnv)
  
  # If external validation dataset exists, predict that;.
  if(0 %in% data$df$fold){
    idx <- which(data$df$fold != 0)
    pred <- ZoonPredict(m,
                        newdata = list(site.covariates = data$site.covariates[idx, , drop = FALSE],
                        obs.covariates = data$obs.covariates[idx, , drop = FALSE]))
    dfOut$predictions[df$fold == 0] <- pred 
    
  }
  
  # Return list of crossvalid and external validation predictions 
  # This list is then the one list that has everything in it.
  out <- list(model = m, data = dfOut, obs.coeffifients = data$obs.covariates, site.coefficients = data$site.covariates)
  return(out)
}


# CheckModList
#
# Helper to sort module arguments into lists with common structure.
#   Want any input to end up as 
#   list(module = moduleName, paras = list(paraName1 = para, paraName2 = 2)).
#   See tests/testthat/testZoon.R for list of potential input.
#@param x A 'call' or 'character' from substitute(occurrence) etc. So either
#    quoted which yields a character substitute('module1') or unquotes which
#    yields a call substitute(module1). 
#@name CheckModList

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



# Split a string into a module and it's arguments
#
# A function that takes a string (from workflow$call) and splits it into a
#   module name and it's arguments.
#
#@param string A string of the form "moduleName" or 
#  "moduleName(parameter = 2, parameter2 = 3)"
#
#@name SplitArgs

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

#FormatModuleList
#
# Little helper to format module lists. Want to return a list
#   newList$module is the module name and newList$paras is a list
#   of the parameters given for the module.
#@param x An object of class 'name' or 'call' e.g. module1 or module1(k=2).
#@name FormatModuleList

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
#' This function is primarily used internally but can be used when
#' running workflows interactively (see vignette \code{basic zoon useage})
#'
#' @param occurrence A data frame from an occurrence module
#' @param ras Environmental raster layer, stack or brick.
#' @export
#' @name ExtractAndCombData

ExtractAndCombData <- function(occurrence, ras){
  
  # Check that all points are within the raster
  bad.coords <- is.na(cellFromXY(ras,
                                 occurrence[,c('longitude', 'latitude')]))
  if(any(bad.coords)){
    occurrence <- occurrence[!bad.coords, ]
    warning ('Some occurrence points are outside the raster extent and have been removed before modelling')
  }
  
  # extract covariates from lat long values in df.
  siteCovariates <- as.data.frame(raster::extract(ras, occurrence[, c('longitude', 'latitude')]))
  
  # extract observation level covariates from occurence data.frame
  idx <- !c("longitude", "latitude","value", "type", "fold") %in% names(occurrence)
  obs.cov.names <- names(occurrence)[idx]
  observationCovariates <- subset(occurrence,select = obs.cov.names)
  
  # Return as list of df and ras as required by process modules
  return(list(df=occurrence, site.covariates=siteCovariates, obs.covariates = observationCovariates, ras=ras))
}


#'Chain modules together
#'
#'\code{Chain} combines multiple modules of the same module type such that they are
#' executed sequentially and their outputs combined.
#' For example, process modules may be \code{Chain}ed to carry out successive
#'  processing operations. By contrast, \code{list}ing modules of the same type
#'  would split the workflow into multiple parallel workflows, each using a
#'  different module at this step.
#' Similarly for occurrence or covariate modules the datasets are joined
#' (row- or layer-wise) whereas \code{list} would carry out separate analyses. 
#' Model and output modules may not be chained. 
#' Developers should note that this function is not actually used - calls using
#'  \code{Chain} are parsed by workflow, with behaviour similar to this function.
#'@param ... List of modules to be chained.
#'@export
#'@name Chain

Chain <- function(...){
  ans <- list(...)
  attr(ans, 'chain') <- TRUE
  return (ans)
}


#SortArgs
#
#
# Helper to take substituted args from workflow call and paste them into 
#   a runeable workflow function.
#@name SortArgs

SortArgs <- function(occSub, covSub, proSub, modSub, outSub, forceReproducible){
  call <- paste0("workflow(", 
                 "occurrence = ", occSub,
                 ", covariate = ", covSub,
                 ", process = ", proSub,
                 ", model = ", modSub,
                 ", output = ", outSub,
                 ", forceReproducible = ", as.character(forceReproducible), ")")
}    



#SplitCall
#
# Helper to split a character string workflow call, as inherited from zoonWorkflow
#   into it's constituent arguments
#@param call A character string of a valid zoon workflow call.
#@name SplitCall

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


# ErrorModule
#
# Function used in tryCatch calls in workflow.
# If an error is caught, return some useful messages.
# Then stop().
# cond is the error messages passed by try catch.
# mod is the modules number (1:5) to give NULLS to the correct modules.
#
#@param e The workflow call environment
#@param cond The error message that was caught in tryCatch.
#@param mod Which module has failed? 1=occurrence, 2=covariate, 3=process
#  4=model, 5=output.
#@name ErrorModule

ErrorModule <- function(cond, mod, e){
  
  # Select the module type using numeric mod argument
  module <- c('occurrence',
              'covariate',
              'process',
              'model',
              'output')[mod]
  
  # Give useful messages.
  # What were the errors that were caught be tryCatch.
  message('Caught errors:\n',  cond)
  message()
  # Where did workflow break and where is the progress stored?
  x <- paste("Stopping workflow due to error in", module, "module.\n", 
             "Workflow progress will be returned.")
  # Throw error. The call for this error is meaningless so don't print it.
  stop(x, call. = FALSE)
}


# PasteAndDep
#
# Paste and deparse. Helper to format substituted args. If arguments were 
#   chained or listed then substitute gives a list. We want to paste it back together.
#@name PasteAndDep

PasteAndDep <- function(x){
  paste(deparse(x), collapse = ' ')
}


# Writeable
# 
# Check whether we can write to a given filepath, throw an error if not.
# Inspired by the is.writeable functionality in assertthat
# @name Writeable
Writeable <- function (dir) {
  OK <- TRUE
  if(!is.character(dir)) OK <- FALSE
  if(!length(dir) == 1) OK <- FALSE
  if(!file.exists(dir)) OK <- FALSE
  if(!file.access(dir, mode = 2)[[1]] == 0) OK <- FALSE
  if(!OK) stop('directory is not writeable ', dir)
}


#' Get MaxEnt
#' 
#' Helper function to get the MaxEnt java executable and install it in
#'  the right locations for zoon modules that use `dismo::maxent` and
#'  `biomod2`.
#'  
#' @details Since MaxEnt may not be distributed other than via the MaxEnt
#' website, users must download the file themselves and place it in the
#' right location for R packages to access it. This function helps with that.
#' Just run \code{GetMaxEnt()} and follow the prompts in the terminal.
#'  
#' @export
#' @name GetMaxEnt
#' @importFrom utils browseURL
GetMaxEnt <- function () {
  # Send the user to download the MaxEnt executable,
  # then find and upload it
  
  # define text
  browser_txt <- "\nTo get MaxEnt working, you'll need to download the executable
  file from the MaxEnt website. The website will require you to give some details,
  once you've done this please download the 'maxent.jar' file to somewhere
  memorable (you'll have to find it again in a second).
  
  Press return to launch the MaxEnt website and continue."
  
  chooser_txt <- "\n\n\n\nzoon now needs to copy the 'maxent.jar' file to the
  correct locations.
  
  Press return to locate the 'maxent.jar' file you just downloaded"
  
  # step one, download the file
  message(browser_txt) # speak to user
  invisible(readline()) # make them hit return
  browseURL('http://www.cs.princeton.edu/~schapire/maxent/') # open the browser
  
  # step two, choose the file
  message(chooser_txt) # speak to user
  invisible(readline()) # make them hit return
  file <- file.choose()
  
  # check it's maxent.jar
  if (basename(file) != 'maxent.jar') {
    stop ("the file selected was not 'maxent.jar'")
  }
  
  # copy to dismo's and biomod2's preferred locations
  dismo_loc <- paste0(system.file(package = 'dismo'), '/java/maxent.jar')
  biomod2_loc <- './maxent.jar'
  dismo_success <- file.copy(file, dismo_loc, overwrite = TRUE)  
  biomod2_success <- file.copy(file, biomod2_loc, overwrite = TRUE)  
  
  # message, warn, or error depending on the level of success
  if (dismo_success) {
    if (biomod2_success) {
      message("maxent.jar successfully deployed for MaxEnt and BiomodModel modules")
    } else {
      warning ("maxent.jar successfully deployed for MaxEnt module, but not for BiomodModel module")
    }
  } else if (biomod2_success) {
    warning ("maxent.jar successfully deployed for BiomodModel module, but not for MaxEnt module")
  } else {
    stop ("maxent.jar not deployed for MaxEnt or BiomodModel modules")
  }
  
}

# AddDefaultParas
# 
# Adds the default parameters and their descriptions to the parameters list in 
# BuildModule.
# @param paras The orginial named list of paramenter descriptions
# @param type The module type used to allocated the defaul arguements
AddDefaultParas <- function(paras, type){
  
  # Define default arguements
  defArgs <- list(occurrence = NULL, covariate = NULL, process = c('.data'),
                  model = c('.df'), output = c('.model', '.ras'))
  
  
  # Remove defaults if they exist, then add in the defaults.
  paras <- paras[!names(paras) %in% defArgs[[type]]]
  
  default_paras <- list(occurrence = NULL,
                        covariate = NULL,
                        process = list(.data = paste("\\strong{Internal parameter, do not use in the workflow function}.",
                                                     "\\code{.data} is a list of a data frame and a raster object returned from", 
                                                     "occurrence modules and covariate modules respectively. \\code{.data} is",
                                                     "passed automatically in workflow from the occurrence and covariate modules",
                                                     "to the process module(s) and should not be passed by the user.")),
                        model = list(.df = paste("\\strong{Internal parameter, do not use in the workflow function}.",
                                                 "\\code{.df} is data frame that combines the occurrence data and",
                                                 "covariate data. \\code{.df} is passed automatically in workflow from",
                                                 "the process module(s) to the model module(s) and should not be",
                                                 "passed by the user.")),
                        output = list(.model = paste("\\strong{Internal parameter, do not use in the workflow function}.",
                                                     "\\code{.model} is list of a data frame (\\code{data}) and a model",
                                                     "object (\\code{model}). \\code{.model} is passed automatically in",
                                                     "workflow, combining data from the model module(s) and process module(s),",
                                                     "to the output module(s) and should not be passed by the user."),
                                      .ras = paste("\\strong{Internal parameter, do not use in the workflow function}.",
                                                   "\\code{.ras} is a raster layer, brick or stack object. \\code{.ras}",
                                                   "is passed automatically in workflow from the covariate module(s) to",
                                                   "the output module(s) and should not be passed by the user.")))
  
  # Add these defaults to the front of the para list
  return(c(default_paras[[type]], paras))
}

# StringToCall
# 
# takes a string and converts it to a call. This is useful for taking the
# call from a workflow that has been run and re-running it.
# @param x The string

StringToCall <- function(x){
  
  parse(text = x[[1]])[[1]]
  
}


# GetModuleVersion
#
# Using the raw text returned from a GetURL call to github
# this extracts the version number

GetModuleVersion <- function(rawText){
  
  # Break down into lines
  ModLines <- strsplit(rawText, '\n')[[1]]
  VersionLine <- ModLines[grep('@section Version: ', ModLines)]
  TagPosition <- gregexpr('@section Version: ', VersionLine)
  Start <- TagPosition[[1]] + attr(TagPosition[[1]], 'match.length')
  substr(VersionLine, Start, nchar(VersionLine))    

}