#' Rerun a workflow object.
#'
#' Takes a workflow object and reruns it.
#'
#' @param workflow A zoonWorkflow object from a previous zoon analysis
#' @param from Which modules should be run. If NULL (default), run from the
#'  first NULL output (i.e. where the workflow broke). Otherwise takes an
#'  integer and runs from that module.
#'
#' @return A list with the results of each module and a copy of the
#'  call used to execute the workflow.
#'
#' @export
#' @name RerunWorkflow
#' @importFrom utils sessionInfo
#' @examples \dontrun{
#' w <- workflow(UKAnophelesPlumbeus,
#'               UKAir,
#'               Background(n = 70),
#'               LogisticRegression,
#'               PrintMap)
#'
#' RerunWorkflow(w)
#' }



RerunWorkflow <- function(workflow, from = NULL) {
  stopifnot(inherits(workflow, "zoonWorkflow"))

  # If from isn't NULL, it should be an integer 1:5
  if (!is.null(from)) {
    stopifnot(from %in% c(1:5))
  }

  # Find first NULL modules and run from there.
  if (is.null(from)) {
    
    NullModules <- vapply(workflow[1:5],
                          is.null,
                          FUN.VALUE = FALSE)
    
    if (any(NullModules)) {
      from <- which.max(NullModules)
    } else {
      from <- 1
    }
  }

  # get the arguments from the call used to run this workflow
  callArgs <- SplitCall(workflow$call)

  # These are strings at first but we need to change them
  # to calls as is expected in the main zoon workflow function
  # this
  occSub <- StringToCall(callArgs["occurrence"])
  covSub <- StringToCall(callArgs["covariate"])
  proSub <- StringToCall(callArgs["process"])
  modSub <- StringToCall(callArgs["model"])
  outSub <- StringToCall(callArgs["output"])

  forceReproducible <- as.logical(callArgs["forceReproducible"])

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

  # Only one of occurrence, covariate, process and model can be a list of
  #   multiple modules.
  module_list <- list(occurrence.module,
                      covariate.module,
                      process.module,
                      model.module,
                      output.module)
  
  isChain <- vapply(module_list,
                    function (x) {
                      isTRUE(attr(x, "chain"))
                    },
                    FUN.VALUE = FALSE)
  
  NoOfModules <- vapply(module_list,
                        length,
                        FUN.VALUE = 0)
  
  if (sum(NoOfModules[!isChain] > 1) > 1)
    stop("Only one module type can be a list of multiple modules.")

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
  
  # Different to workflow(), We have an if statement before each module is run
  #   to check the 'from' argument.

  # Run the modules. (these functions are in DoModuleFunctions.R)
  # But we have to check for chained modules and deal with them
  # And work out which module has been given as a list, and lapply over that.

  # Each module is in trycatch.
  # If a module breaks we want to save the progress so far and let the user
  # know which module broke.


  # set up object to return on error

  # Collate output
  output <- list(
    occurrence.output = NULL,
    covariate.output = NULL,
    process.output = NULL,
    model.output = NULL,
    report = NULL,
    call = workflow$call,
    call.list = workflow$call.list,
    session.info = session.info,
    module.versions = moduleVersions
  )

  class(output) <- "zoonWorkflow"

  # whether exiting on error, or successful completion, return this
  on.exit(return(output))

  # First the data collection modules
  # Actually tryCatch here only tells user which module broke, nothing to save.
  if (from <= 1) {
    tryCatch(
      {
        occurrence.output <- lapply(occurrenceName,
                                    FUN = DoOccurrenceModule,
                                    e)
        # Then bind together if the occurrence modules were chained
        if (identical(attr(occurrence.module, "chain"), TRUE)) {
          occurrence.output <- list(do.call(rbind, occurrence.output))
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
  } else {
    occurrence.output <- workflow$occurrence.output
    output$occurrence.output <- occurrence.output
  }

  if (from <= 2) {
    tryCatch(
      {
        covariate.output <- lapply(covariateName, FUN = DoCovariateModule, e)
        if (identical(attr(covariate.module, "chain"), TRUE)) {
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
  } else {
    covariate.output <- workflow$covariate.output
    output$covariate.output <- covariate.output
  }


  # Simply combine data into basic df shape
  # This shape is then input and output of all process modules.
  # Also makes it easy to implement a NULL process

  if (length(covariateName) > 1) {
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



  if (from <= 3) {
    tryCatch(
      {
        process.output <- DoProcessModules(process.module, processName, data, e)
        output$process.output <- process.output
      },
      error = function(cond) {
        ErrorModule(cond, 3, e)
      }
    )
  } else {
    process.output <- workflow$process.output
    output$process.output <- process.output
  }


  # Model module
  if (from <= 4) {
    tryCatch(
      {
        model.output <- DoModelModules(model.module,
                                       modelName,
                                       process.output,
                                       e)
        output$model.output <- model.output
      },
      error = function(cond) {
        ErrorModule(cond, 4, e)
      }
    )
  } else {
    model.output <- workflow$model.output
    output$model.output <- model.output
  }
  # output module
  # If output isn't chained, might have to lapply over
  #   output, covariate or process
  # If output is chained, either covariate or process only.
  #  Within this need to chain output

  if (from <= 5) {
    tryCatch(
      {
        output.output <- DoOutputModules(
          output.module, outputName,
          process.module, process.output, model.output, e
        )
        output$report <- output.output
      },
      error = function(cond) {
        ErrorModule(cond, 5, e)
      }
    )
  } else {
    output.output <- workflow$report
    output$report <- output.output
  }
}
