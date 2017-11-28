
#' A function to load a module function from url or disk.
#'
#' Loads a module function into the global environment ready to be used in a
#'  zoon workflow. This function is mostly for use while developing modules.
#'  Workflows run with modules defined locally are no longer reproducible and
#'  so are discouraged and will be tagged as 'unreproducible'.
#'
#' @param module A string that describes the location of the R file. Can be a
#'  a full URL or a path to a local file.
#'
#' @return Name of the function. Adds function to global namespace.
#' @name LoadModule
#' @export

LoadModule <- function(module) {

  # module must be a string

  # URL to module in user's favourite repo & branch
  zoonURL <- sprintf(
    "%s/%s/R/%s.R",
    options("zoonRepo"),
    options("zoonRepoBranch"),
    module
  )

  # If module is a path, load module
  if (file.exists(module)) {
    txt <- parse(text = paste(readLines(module), collapse = "\n"))
    # If zoonURL is a zoon repo url, load module Could probably do same thing as
    # GetModule here to avoid repeated web call
  } else if (url.exists(zoonURL, .opts = list(ssl.verifypeer = FALSE))) {
    txt <- parse(text = getURL(zoonURL, ssl.verifypeer = FALSE))
    # If module on its own is a url, load module
  } else if (url.exists(module, .opts = list(ssl.verifypeer = FALSE))) {
    txt <- parse(text = getURL(module, ssl.verifypeer = FALSE))
    # Otherwise throw error.
  } else {
    modList <- GetModuleList()
    module_idx <- agrep(module, unlist(modList), max.distance = 0.3)
    closeMatches <- unlist(modList)[module_idx]
    if (length(closeMatches) == 0) {
      stop ("Can't find '", module,
            "' or any modules with closely matching names.")
    } else if (length(closeMatches) == 1) {
      stop ("Can't find '", module,
            "'. Did you mean '", closeMatches, "'?")
    } else {
      stop ("Can't find '", module, "'. Did you mean one of '",
            paste(closeMatches, collapse = "', "), "'?")
    }
  }
  # Load to global environment
  eval(txt, envir = globalenv())
  # Return the actual name of the module that has been loaded.
  #   Don't just return 'module' argument as that can be url/path
  #   which isn't useful.
  # Cruddy code. But module name is the only other object in this
  #   call environment.
  eval(txt)
  new.func.name <- ls()[!ls() %in% c("module", "txt", "zoonURL")]
  return(new.func.name)
}



# A function to get a module function.
#
# Checks for the module in the global namespace, then the zoon repo. Then reads
#  the module source and loads the function.
#
# @param module A string that describes the location of the R file. Can be a
#      module name assuming the module is in global namespace or
#      github.com/zoonproject/modules. Otherwise can be a full URL or a local
#      file.
# @param forceReproducible Do we want to force the function to get modules from
#      the zoon repo, even if they exist in the global namespace.
#
# @return Name of the function. Function is run with workflow and new module
#  function is added to workflow environment.
# @name GetModule

GetModule <- function(module, forceReproducible, environment = parent.frame()) {

  # URL to module in user's favourite repo & branch
  zoonURL <- sprintf(
    "%s/%s/R/%s.R",
    options("zoonRepo"),
    options("zoonRepoBranch"),
    module
  )

  # If the module is in global namespace, use that function
  #   unless forceReproduce is TRUE, in which case we want to get from repo.
  #
  # Get module from zoonURL otherwise.
  module_exists <- exists(module,
                          where = ".GlobalEnv",
                          mode = "function",
                          inherits = FALSE)
  if (module_exists & !forceReproducible) {
    assign(module,
           eval(parse(text = module), envir = globalenv()),
           envir = environment)
    attr(module, "version") <- "local copy"
    return(module)
  } else {
    rawText <- getURL(zoonURL, ssl.verifypeer = FALSE)
  }

  # getURL returns "Not Found" if no webpage found.
  #   Use this to avoid two web call.s
  if (grepl("^404: Not Found", rawText)) {
    stop('Cannot find "', module,
         '". Check that the module is on the zoon repository ',
         'or in the global namespace.')
  }

  # Parse text from webpage.
  txt <- parse(text = rawText)

  # Evaluate text in the workflow call environment
  eval(txt, envir = environment)

  # Assign version attribute
  attr(module, "version") <- GetModuleVersion(rawText)

  return(module)
}


# A function to apply GetModule to a list correctly.
# @param modules A list from CheckModList() given details for
#  one or more modules.
# @param forceReproducible Logical to determine whether the modules should be
#  taken from repo even if they exist locally to enforce reproducibility.
# @name LapplyGetModule

LapplyGetModule <- function (modules,
                             forceReproducible,
                             environment = parent.frame()) {
  lapply(modules,
         function(x) {
           GotModule <- GetModule(as.character(x$module),
                                  forceReproducible,
                                  environment)
           res <- c(x,
                    func = GotModule,
                    version = attr(GotModule, "version"))
           return (res)
         })
}


#' RunModels
#'
#' A function to train and predict crossvalidation folds and train one full
#' model and predict any external validation data. This function is primarily
#' used internally but can be used when running workflows interactively (see
#' vignette \code{basic zoon useage})
#'
#' @param df Dataframe from process module. Should contain columns value, type,
#'   lon, lat and fold, a number indicating which cross validation set a
#'   datapoint is in. If fold is 0 then this is considered external validation
#'   data If all data is 0 or 1, then no cross validation is run.
#'
#' @param modelFunction String giving the name of the model function which is in
#'   turn the name of the module.
#'
#' @param paras All other parameters that should be passed to the model
#'   function. i.e. model[[1]]$paras
#'
#' @param workEnv The environment name of the workflow call environment.
#'
#' @return A list of length 2 containing the model trained on all data and a
#'   data.frame which contains value, type, fold, lon, lat, predictions and then
#'   all environmental variables.
#' @export
#' @name RunModels

RunModels <- function(df, modelFunction, paras, workEnv) {
  # Count non zero folds
  # 0 are for external validation only.
  k <- length(unique(df$fold)[unique(df$fold) != 0])

  # Doing predictions requires handling of NAs in subsets
  # of the data (subsets = folds). This breaks if there
  # is already a na.action attribute on df, so we remove it
  if ("na.action" %in% names(attributes(df)))
    attributes(df) <- attributes(df)[!names(attributes(df)) %in% "na.action"]

  # Init. output dataframe with predictions column
  # Old versions of modules dont use this attribute
  ## REMOVE ONCE MODULES UPDATED ##
  if ("covCols" %in% names(attributes(df))) {
    dfOut <- cbindZoon(
      subsetColumnsZoon(df, !colnames(df) %in% attr(df, "covCols")),
      cbind(predictions = NA, df[colnames(df) %in% attr(df, "covCols")])
    )
  } else {
    dfOut <- cbind(df[, 1:5], predictions = NA, df[, 6:NCOL(df)])
    names(dfOut)[7:ncol(dfOut)] <- names(df)[6:ncol(df)]
  }

  # We don't know that they want cross validation.
  # If they do, k>1, then run model k times and predict out of bag
  # Skip otherwise
  if (k > 1) {
    for (i in 1:k) {
      modelFold <- do.call(
        modelFunction, c(
          .df = list(df[df$fold != i, ]),
          paras
        ),
        envir = workEnv
      )

      # Old versions of modules dont use this attribute
      ## REMOVE ONCE MODULES UPDATED ##
      if ("covCols" %in% names(attributes(df))) {
        pred <- ZoonPredict(
          modelFold,
          newdata = subsetColumnsZoon(
            df[df$fold == i, ],
            attr(df, "covCols")
          )
        )
      } else {
        pred <- ZoonPredict(
          modelFold,
          newdata = df[df$fold == i, 6:NCOL(df), drop = FALSE]
        )
      }

      dfOut$predictions[df$fold == i] <- pred
    }
  }

  # Run model on all data except external validation data
  m <- do.call(
    modelFunction, c(.df = list(df[df$fold != 0, ]), paras),
    envir = workEnv
  )

  # If external validation dataset exists, predict that;.
  if (0 %in% df$fold) {

    # Old versions of modules dont use this attribute
    ## REMOVE ONCE MODULES UPDATED ##
    if ("covCols" %in% names(attributes(df))) {
      pred <- ZoonPredict(
        m,
        newdata = subsetColumnsZoon(
          df[df$fold == 0, ],
          attr(df, "covCols")
        )
      )
    } else {
      pred <- ZoonPredict(
        m,
        newdata = df[df$fold == 0, 6:NCOL(df), drop = FALSE]
      )
    }

    dfOut$predictions[df$fold == 0] <- pred
  }

  # Return list of crossvalid and external validation predictions
  # This list is then the one list that has everything in it.
  out <- list(model = m, data = dfOut)
  return(out)
}




# CheckModList
#
# Helper to sort module arguments into lists with common structure.
#   Want any input to end up as
#   list(module = moduleName, paras = list(paraName1 = para, paraName2 = 2)).
#   See tests/testthat/testZoon.R for list of potential input.
# @param x A 'call' or 'character' from substitute(occurrence) etc. So either
#    quoted which yields a character substitute('module1') or unquotes which
#    yields a call substitute(module1).
# @name CheckModList

# Check passing as quoted. e.g. occurrence = "ModuleName(k=2)"
# Also occurrence = "list(mod1, mod2)" is probably bad.

CheckModList <- function(x) {

  # Should accept occurrence = 'module1', but NOT
  #   occurrence = 'module1(k=2)', or occurrence = 'list(mod1, mod1)'
  if (inherits(x, "character")) {
    if (grepl("[^\' | ^\"]", x) & grepl("[( | )]", x)) {
      stop(paste(
        "If specifying module arguments please use the form",
        "Module(para = 2), without quotes. No special characters should exist",
        "in module names."
      ))
    }
  }

  # If argument is passed as unquoted moduleName: occurrence = ModuleName,
  if (class(x) == "name") {
    ModuleList <- list(list(module = as.character(x), paras = list()))

    # If list of modules given: occurrence = list(Mod1, Mod2),
    #   If list(Mod1(k=2), Mod2(p = 3)), parameters sorted in
    #   FormatModuleList
  } else if (x[[1]] == "list") {
    listCall <- as.list(x)
    listCall[[1]] <- NULL
    ModuleList <- lapply(listCall, FormatModuleList)

    # If Chained modules given: occurrence = Chain(Mod1, Mod2),
  } else if (x[[1]] == "Chain") {
    listCall <- as.list(x)
    listCall[[1]] <- NULL
    ModuleList <- lapply(listCall, FormatModuleList)
    attr(ModuleList, "chain") <- TRUE

    # If unquoted module w/ paras given: occurrence = Module1(k=2)
  } else if (x[[1]] == "Replicate") {
    listCall <- eval(x)

    ModuleList <- lapply(listCall, FormatModuleList)

    # If unquoted module w/ paras given: occurrence = Module1(k=2)
  } else if (identical(class(x[[1]]), "name")) {
    # Parameters
    paras <- as.list(x)
    paras[[1]] <- NULL
    ModuleList <- list(list(module = as.character(x[[1]]), paras = paras))
    # Deal with all quoted forms
    #   Can include 'module1', 'module(para = 2)', 'module(p = 2, q = 'wer')'
  } else if (inherits(x, "character")) {
    ModuleList <- list(SplitArgs(x))
  } else {
    stop(paste("Please check the format of argument", as.character(x)))
  }

  return(ModuleList)
}



# Split a string into a module and it's arguments
#
# A function that takes a string (from workflow$call) and splits it into a
#   module name and it's arguments.
#
# @param string A string of the form "moduleName" or
#  "moduleName(parameter = 2, parameter2 = 3)"
#
# @name SplitArgs

SplitArgs <- function(string) {
  module <- gsub("(^)(.*)(\\(.*$)", "\\2", string)
  if (grepl("\\(", string)) {
    args <- gsub("(^.*\\()(.*)(\\)$)", "\\2", string)
  } else {
    args <- ""
  }
  sepArgs <- (strsplit(args, ","))[[1]]
  arguments <- lapply(
    strsplit(sepArgs, "="),
    function(x) gsub(" ", "", x[2])
  )
  names(arguments) <- unlist(lapply(
    strsplit(sepArgs, "="),
    function(x) gsub(" ", "", x[1])
  ))
  return(list(module = module, paras = arguments))
}

# FormatModuleList
#
# Little helper to format module lists. Want to return a list
#   newList$module is the module name and newList$paras is a list
#   of the parameters given for the module.
# @param x An object of class 'name' or 'call' e.g. module1 or module1(k=2).
# @name FormatModuleList

FormatModuleList <- function(x) {
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

ExtractAndCombData <- function(occurrence, ras) {

  # Check for rows in the occurrence data that have missing data
  NArows <- apply(
    occurrence[, c("longitude", "latitude")],
    1,
    function(x) any(is.na(x))
  )

  if (any(NArows)) {
    warning (sum(NArows), " row(s) of occurrence data have NA values ",
             "for latitude/longitude and will be removed")
    occurrence <- occurrence[!NArows, ]
  }

  if (is.na(projection(ras))) {
    
    message("Covariate raster does not have a projection, zoon will assume ",
            "this is in the same projection as your occurrence data")
    
  } else if ("crs" %in% tolower(colnames(occurrence))) {
    occurrence <- TransformCRS(
      occurrence = occurrence,
      ras_projection = projection(ras)
    )
  }

  # Check that all points are within the raster
  bad.coords <- is.na(cellFromXY(
    ras,
    occurrence[, c("longitude", "latitude")]
  ))
  if (any(bad.coords)) {
    nr_before <- nrow(occurrence)
    occurrence <- occurrence[!bad.coords, ]
    nr_after <- nrow(occurrence)

    if (nr_after > 0) {
      warning (nr_before - nr_after, "occurrence points are outside ",
               "the raster extent and have been removed before modelling ",
               "leaving", nr_after, "occurrence points")
    } else if (nr_after == 0) {
      warning ("All occurrence points are outside the raster extent. ",
               "Try changing your raster.")
    }
  }

  # extract covariates from lat long values in df.
  ras.values <- raster::extract(ras,
                                occurrence[, c("longitude", "latitude")])
  if (is.null(ras.values)) {
    occurrenceCovariates <- NULL
    warning ("Locations in the occurrence data did not match your raster ",
             "so no covariate data were extracted. ",
             "This is only a good idea if you are creating simulated data ",
             "in the process module")
  } else {
    if (any(is.na(ras.values))) {
      warning(sum(is.na(ras.values)), "extracted covariate values are NA. ",
              "This may cause issues for some models")
    }
    occurrenceCovariates <- as.matrix(ras.values)
    colnames(occurrenceCovariates) <- names(ras)
  }

  df <- cbindZoon(occurrence, occurrenceCovariates)

  # assign call_path attribute to this new object
  attr(df, "call_path") <- attr(occurrence, "call_path")

  # record the covariate column names
  attr(df, "covCols") <- names(ras)

  # Return as list of df and ras as required by process modules
  return(list(df = df, ras = ras))
}


#' Chain modules together
#'
#' \code{Chain} combines multiple modules of the same module type such that they
#' are executed sequentially and their outputs combined. For example, process
#' modules may be \code{Chain}ed to carry out successive processing operations.
#' By contrast, \code{list}ing modules of the same type would split the workflow
#' into multiple parallel workflows, each using a different module at this step.
#'
#' Similarly for occurrence or covariate modules the datasets are joined (row-
#' or layer-wise) whereas \code{list} would carry out separate analyses. Model
#' and output modules may not be chained. Developers should note that this
#' function is not actually used - calls using \code{Chain} are parsed by
#' workflow, with behaviour similar to this function.
#' 
#' @param ... List of modules to be chained.
#' @export
#' @name Chain

Chain <- function(...) {
  ans <- list(...)
  attr(ans, "chain") <- TRUE
  return(ans)
}


# SortArgs
#
#
# Helper to take substituted args from workflow call and paste them into
#   a runeable workflow function.
# @name SortArgs

SortArgs <- function (occSub,
                      covSub,
                      proSub,
                      modSub,
                      outSub,
                      forceReproducible) {
  call <- paste0(
    "workflow(",
    "occurrence = ", occSub,
    ", covariate = ", covSub,
    ", process = ", proSub,
    ", model = ", modSub,
    ", output = ", outSub,
    ", forceReproducible = ", as.character(forceReproducible),
    ")"
  )
}



# SplitCall
#
# Helper to split a character string workflow call, as inherited from
# zoonWorkflow into it's constituent arguments
# @param call A character string of a valid zoon workflow call.
# @name SplitCall

SplitCall <- function(call) {

  # Regex to find each argument within call.
  #   Find 3 patterns and sub whole string with just middle pattern
  #   Middle pattern is the argument name.
  occurrence <- gsub("(.*occurrence = )(.*)(, covariate.*$)", "\\2", call)
  covariate <- gsub("(.*covariate = )(.*)(, process.*$)", "\\2", call)
  process <- gsub("(.*process = )(.*)(, model.*$)", "\\2", call)
  model <- gsub("(.*model = )(.*)(, output.*$)", "\\2", call)
  output <- gsub("(.*output = )(.*)(, forceReproducible.*$)", "\\2", call)
  forceReproducible <- gsub("(.*forceReproducible = )(.*)())", "\\2", call)

  # Make vector and add names.
  split <- c(occurrence, covariate, process, model, output, forceReproducible)
  names(split) <- c(
    "occurrence", "covariate", "process",
    "model", "output", "forceReproducible"
  )

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
# @param e The workflow call environment
# @param cond The error message that was caught in tryCatch.
# @param mod Which module has failed? 1=occurrence, 2=covariate, 3=process
#  4=model, 5=output.
# @name ErrorModule

ErrorModule <- function(cond, mod, e) {

  # Select the module type using numeric mod argument
  module <- c(
    "occurrence module.",
    "covariate module.",
    paste("ExtractAndCombData, a function that combines",
          "occurrence and covariate data."),
    "process module.",
    "model module.",
    "output module."
  )[mod]

  # Give useful messages.
  # What were the errors that were caught by tryCatch.
  message("Caught errors:\n", cond)
  message()
  # Where did workflow break and where is the progress stored?
  x <- paste("Stopping workflow due to error in", module, "\n")
  # Throw error. The call for this error is meaningless so don't print it.
  stop(x, call. = FALSE)
}


# PasteAndDep
#
# Paste and deparse. Helper to format substituted args. If arguments were
# chained or listed then substitute gives a list. We want to paste it back
# together.
# @name PasteAndDep

PasteAndDep <- function(x) {
  paste(deparse(x), collapse = " ")
}


# Writeable
#
# Check whether we can write to a given filepath, throw an error if not.
# Inspired by the is.writeable functionality in assertthat
# @name Writeable
Writeable <- function(dir) {
  OK <- TRUE
  if (!is.character(dir)) OK <- FALSE
  if (!length(dir) == 1) OK <- FALSE
  if (!file.exists(dir)) OK <- FALSE
  if (!file.access(dir, mode = 2)[[1]] == 0) OK <- FALSE
  if (!OK) stop("directory is not writeable ", dir)
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
GetMaxEnt <- function() {
  # Send the user to download the MaxEnt executable,
  # then find and upload it

  # define text
  browser_txt <- paste("\nTo get MaxEnt working, you'll need to download the",
                       "executable file from the MaxEnt website. The website",
                       "will require you to give some details, once you've",
                       "done this please download the 'maxent.jar' file to",
                       "somewhere memorable (you'll have to find it again in",
                       "a second).\n\nPress return to launch the MaxEnt",
                       "website and continue.")

  chooser_txt <- paste("\n\n\n\nzoon now needs to copy the 'maxent.jar' file",
                       "to the correct locations.\n\nPress return to locate",
                       "the 'maxent.jar' file you just downloaded")

  # step one, download the file
  message(browser_txt) # speak to user
  invisible(readline()) # make them hit return
  browseURL("http://www.cs.princeton.edu/~schapire/maxent/") # open the browser

  # step two, choose the file
  message(chooser_txt) # speak to user
  invisible(readline()) # make them hit return
  file <- file.choose()

  # check it's maxent.jar
  if (basename(file) != "maxent.jar") {
    stop("the file selected was not 'maxent.jar'")
  }

  # copy to dismo's and biomod2's preferred locations
  dismo_loc <- paste0(system.file(package = "dismo"), "/java/maxent.jar")
  biomod2_loc <- "./maxent.jar"
  dismo_success <- file.copy(file, dismo_loc, overwrite = TRUE)
  biomod2_success <- file.copy(file, biomod2_loc, overwrite = TRUE)

  # message, warn, or error depending on the level of success
  if (dismo_success) {
    if (biomod2_success) {
      message("maxent.jar successfully deployed for MaxEnt ",
              "and BiomodModel modules")
    } else {
      warning ("maxent.jar successfully deployed for MaxEnt module, ",
               "but not for BiomodModel module")
    }
  } else if (biomod2_success) {
    warning ("maxent.jar successfully deployed for BiomodModel module, ",
             "but not for MaxEnt module")
  } else {
    stop ("maxent.jar not deployed for MaxEnt or BiomodModel modules")
  }
}

# AddDefaultParas
#
# Adds the default parameters and their descriptions to the parameters list in
# BuildModule.
# @param paras The orginial named list of paramenter descriptions
# @param type The module type used to allocated the default arguements
AddDefaultParas <- function(paras, type) {

  # Define default arguements
  defArgs <- list(
    occurrence = NULL, covariate = NULL, process = c(".data"),
    model = c(".df"), output = c(".model", ".ras")
  )

  # Remove defaults if they exist, then add in the defaults.
  paras <- paras[!names(paras) %in% defArgs[[type]]]

  default_paras <- list(
    occurrence = NULL,
    covariate = NULL,
    process = list(.data = paste(
      "\\strong{Internal parameter, do not use in the workflow function}.",
      "\\code{.data} is a list of a data frame and a raster object returned",
      "from occurrence modules and covariate modules respectively.",
      "\\code{.data} is passed automatically in workflow from the occurrence",
      "and covariate modules to the process module(s) and should not be passed",
      "by the user."
    )),
    model = list(.df = paste(
      "\\strong{Internal parameter, do not use in the workflow function}.",
      "\\code{.df} is data frame that combines the occurrence data and",
      "covariate data. \\code{.df} is passed automatically in workflow from",
      "the process module(s) to the model module(s) and should not be",
      "passed by the user."
    )),
    output = list(
      .model = paste(
        "\\strong{Internal parameter, do not use in the workflow function}.",
        "\\code{.model} is list of a data frame (\\code{data}) and a model",
        "object (\\code{model}). \\code{.model} is passed automatically in",
        "workflow, combining data from the model module(s) and process",
        "module(s) to the output module(s) and should not be passed by",
        "the user."
      ),
      .ras = paste(
        "\\strong{Internal parameter, do not use in the workflow function}.",
        "\\code{.ras} is a raster layer, brick or stack object. \\code{.ras}",
        "is passed automatically in workflow from the covariate module(s) to",
        "the output module(s) and should not be passed by the user."
      )
    )
  )

  # Add these defaults to the front of the para list
  return(c(default_paras[[type]], paras))
}

# StringToCall
#
# takes a string and converts it to a call. This is useful for taking the
# call from a workflow that has been run and re-running it.
# @param x The string

StringToCall <- function(x) {
  parse(text = x[[1]])[[1]]
}


# GetModuleVersion
#
# Using the raw text returned from a GetURL call to github
# this extracts the version number

GetModuleVersion <- function(rawText) {

  # Break down into lines
  ModLines <- strsplit(rawText, "\n")[[1]]
  VersionLine <- ModLines[grep("@section Version: ", ModLines)]
  TagPosition <- gregexpr("@section Version: ", VersionLine)
  Start <- TagPosition[[1]] + attr(TagPosition[[1]], "match.length")
  substr(VersionLine, Start, nchar(VersionLine))
}


# tryCatchModule
#
# This function runs a call to a module in testing and handles
# error that may occur in a way to make debugging easier

tryCatchModule <- function(expr, code_chunk, fun, debug = TRUE) {
  tryCatch(
    expr = expr,
    error = function(err, func = fun, debug_f = debug) {
      error_message <- paste(
        "\nYour module failed to run with default parameters\n",
        "ERROR:", err,
        "\nYou can debug this error by running the following code chunk",
        "\n===========\n",
        ifelse(test = debug_f,
               yes = paste0("debugonce(", func, ")\n"),
               no = ""),
        paste(code_chunk, collapse = "\n"),
        "\n==========="
      )
      class(error_message) <- "moduleError"
      return(error_message)
    }
  )
}

# tryCatchWorkflow
#
# This function runs a workflow in testing and handles
# error that may occur in a way to make debugging easier

tryCatchWorkflow <- function(expr, placeholder, fun) {
  
  code_temp <- paste0(trimws(capture.output(print(substitute(expr)))))
  code_chunk <- gsub(placeholder, fun, trimws(gsub("[{}]", "", code_temp)))

  tryCatchModule(
    expr = expr, code_chunk = code_chunk,
    fun = fun, debug = FALSE
  )
}
