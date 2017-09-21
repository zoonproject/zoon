
#' Run a full workflow.
#'
#' This is the main function of zoon. The arguments should specify at least five
#' modules, at least one of each type.
#' If modules do not have any arguments to be specific (or defaults are being
#' used) then simply give the names of the module. If arguments are needed
#' give the modules in the form of a function
#' e.g. occurrence = AModule(para1 = 2, para2 = 'detail')
#'
#' @param occurrence Occurrence module to be used.
#' @param covariate  Covariate module to be used.
#' @param process Process module to be used.
#' @param model SDM model module to be used.
#' @param output Output module to be used.
#' @param forceReproducible Logical whether to force zoon to collect modules
#'  from the online repo. This ensure the analysis is reproducible.
#'
#' @return A list with the results of each module and a copy of the
#'  code used to execute the workflow. If the workflow fails a partial
#'  list is saved to a temporary file for debugging.
#' @export
#' @name workflow
#' @importFrom utils sessionInfo
#' @importFrom plyr rbind.fill
#' @examples
#' # run a workflow, using the logistic regression model
#' \dontrun{
#'
#' work1 <- workflow(occurrence = UKAnophelesPlumbeus,
#'                 covariate = UKAir,
#'                 process = Background(n = 70),
#'                 model = LogisticRegression,
#'                 output = SameTimePlaceMap)
#'
#' str(work1, 1)
#'
#' work2 <- workflow(UKAnophelesPlumbeus,
#'                  UKAir,
#'                  OneHundredBackground,
#'                  RandomForest,
#'                  PrintMap)
#'
#' }

workflow <- function(occurrence,
                     covariate,
                     process,
                     model,
                     output,
                     forceReproducible = FALSE) {
  
  occSub <- substitute(occurrence)
  covSub <- substitute(covariate)
  proSub <- substitute(process)
  modSub <- substitute(model)
  outSub <- substitute(output)

  call <- SortArgs(
    PasteAndDep(occSub), PasteAndDep(covSub), PasteAndDep(proSub),
    PasteAndDep(modSub), PasteAndDep(outSub), forceReproducible
  )

  # save the local environment as it needs to be passed to various functions.
  e <- new.env(parent = parent.frame())

  # capture the session info to return in workflow object
  session.info <- sessionInfo()

  # Check all modules are of same list structure
  occurrence.module <- CheckModList(occSub)
  covariate.module <- CheckModList(covSub)
  process.module <- CheckModList(proSub)
  model.module <- CheckModList(modSub)
  output.module <- CheckModList(outSub)

  # create a list of these things to return
  call.list <- list(
    occurrence.module,
    covariate.module,
    process.module,
    model.module,
    output.module
  )

  # Only one of occurrence, covariate, process and model can be a list of
  #   multiple modules. But ignore chained modules.
  isChain <- vapply(call.list,
                    function (x) {
                      isTRUE(attr(x, "chain"))
                    },
                    FUN.VALUE = FALSE)
  
  NoOfModules <- vapply(call.list,
                        length,
                        FUN.VALUE = 0)
  
  if (sum(NoOfModules[!isChain] > 1) > 1)
    stop ("Only one module type can be a list of multiple modules.")

  # Get the modules (functions) from github.
  # Save name of functions as well as load functions into global namespace.
  # Will probably want to make this so it checks namespace first.
  occurrenceName <- LapplyGetModule(occurrence.module, forceReproducible, e)
  covariateName <- LapplyGetModule(covariate.module, forceReproducible, e)
  processName <- LapplyGetModule(process.module, forceReproducible, e)
  # Check for val type lon lat covs
  modelName <- LapplyGetModule(model.module, forceReproducible, e)
  # Test for predict method
  outputName <- LapplyGetModule(output.module, forceReproducible, e)

  fun_ver <- function(x) c(module = x$func, version = x$version)
  eg <- c(module = "a", version = "b")
  
  # Build module version list
  moduleVersions <- list(
    occurrence = vapply(occurrenceName, fun_ver, FUN.VALUE = eg),
    covariate = vapply(covariateName, fun_ver, FUN.VALUE = eg),
    process = vapply(processName, fun_ver, FUN.VALUE = eg),
    model = vapply(modelName, fun_ver, FUN.VALUE = eg),
    output = vapply(outputName, fun_ver, FUN.VALUE = eg)
  )
  
  # Run the modules. (these functions are in DoModuleFunctions.R)
  # But we have to check for chained modules and deal with them
  # And work out which module has been given as a list, and lapply over that.

  # Each module is in trycatch.
  # If a module breaks we want to save the progress so far and let the user
  # know which module broke.

  # First the data collection modules
  # Actually tryCatch here only tells user which module broke, nothing to save.

  # If you want to parallelise modules, (properly on multicores), lapply will
  #   become snowfall::sflapply or newer things.

  # set up zoon object now so we can return it if there's an error

  output <- list(
    occurrence.output = NULL,
    covariate.output = NULL,
    process.output = NULL,
    model.output = NULL,
    report = NULL,
    call = call,
    call.list = call.list,
    session.info = session.info,
    module.versions = moduleVersions
  )

  class(output) <- "zoonWorkflow"

  # whether exiting on error, or successful completion, return this
  on.exit(expr = {
    tempf <- tempfile(fileext = ".rdata")
    save(output, file = tempf)
    message(paste0(
      "The process failed. The partially completed workflow has been ",
      "saved as a temporary file. Load the partially completed workflow",
      'named "output" by using load("',
      normalizePath(tempf, winslash = "/"), '")'
    ))
  })


  # Run the occurrence modules
  tryCatch(
    {
      occurrence.output <- lapply(occurrenceName,
                                  FUN = DoOccurrenceModule,
                                  e = e)
      # Then bind together if the occurrence modules were chained
      if (identical(attr(occurrence.module, "chain"), TRUE)) {
        occurrence.output <- list(do.call(rbind.fill, occurrence.output))
        attr(occurrence.output[[1]], "call_path") <- list(occurrence = paste(
          "Chain(",
          paste(
            lapply(occurrenceName, function(x) x$module),
            collapse = ", "
          ),
          ")", sep = ""
        ))
      }
      output$occurrence.output <- occurrence.output
    },
    error = function(cond) {
      ErrorModule(cond, 1, e)
    }
  )

  # Run to covariate modules
  tryCatch(
    {
      covariate.output <- lapply(covariateName,
                                 FUN = DoCovariateModule,
                                 e = e)
      if (identical(attr(covariate.module, "chain"), TRUE)) {
        covariate.output <- CombineRasters(covariate.output)
        covariate.output <- list(do.call(raster::stack, covariate.output))
        attr(covariate.output[[1]], "call_path") <- list(covariate = paste(
          "Chain(",
          paste(
            lapply(covariateName, function(x) x$module),
            collapse = ", "
          ),
          ")", sep = ""
        ))
      }
      output$covariate.output <- covariate.output
    },
    error = function(cond) {
      ErrorModule(cond, 2, e)
    }
  )

  # Simply combine data into basic df shape
  # This shape is then input and output of all process modules.
  # Also makes it easy to implement a NULL process
  tryCatch(
    {
      if (length(covariate.output) > 1) {
        data <- lapply(
          covariate.output,
          function(x) ExtractAndCombData(occurrence.output[[1]], x)
        )
      } else {
        data <- lapply(
          occurrence.output,
          function(x) ExtractAndCombData(x, covariate.output[[1]])
        )
      }
    },
    error = function(cond) {
      ErrorModule(cond, 3, e)
    }
  )

  # Do process modules

  tryCatch(
    {
      process.output <- DoProcessModules(process.module,
                                         processName,
                                         data,
                                         e)
      output$process.output <- process.output
    },
    error = function(cond) {
      ErrorModule(cond, 4, e)
    }
  )

  # Model module
  tryCatch(
    {
      model.output <- DoModelModules(model.module,
                                     modelName,
                                     process.output,
                                     e)
      output$model.output <- model.output
    },
    error = function(cond) {
      ErrorModule(cond, 5, e)
    }
  )
  # output module
  # If output isn't chained, might have to lapply over
  #   output, covariate or process
  # If output is chained, either covariate or process only.
  #  Within this need to chain output
  tryCatch(
    {
      output.output <- DoOutputModules(
        output.module, outputName,
        process.module, process.output, model.output, e
      )
      output$report <- output.output
    },
    error = function(cond) {
      ErrorModule(cond, 6, e)
    }
  )

  on.exit()
  return(output)
}
