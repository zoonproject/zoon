#' Change a workflow and rerun.
#'
#' Takes a workflow object and reruns it with changes.
#'
#' @param workflow A zoonWorkflow object from a previous zoon analysis
#' @param occurrence,covariate,process,model,output Optional modules (or lists or Chains) to
#'  replace the modules in \code{workflow}
#' @param forceReproducible Optional logical. Whether to force zoon to collect modules
#'  from the online repo in the new workflow. This ensure the analysis is reproducible.
#' @return A list with the results of each module and a copy of the
#'  call used to execute the workflow (
#'
#' @export
#' @name ChangeWorkflow
#' @importFrom utils sessionInfo
#' @examples \dontrun{
#' w <- workflow(UKAnophelesPlumbeus,
#'               UKAir,
#'               Background(n = 70),
#'               LogisticRegression,
#'               PrintMap)
#'
#' w2 <- ChangeWorkflow(w,
#'                      output = PrintMap)
#' }

ChangeWorkflow <- function(workflow,
                           occurrence = NULL,
                           covariate = NULL,
                           process = NULL,
                           model = NULL,
                           output = NULL,
                           forceReproducible = NULL) {

  # Sub all inputs
  occSub <- substitute(occurrence)
  covSub <- substitute(covariate)
  proSub <- substitute(process)
  modSub <- substitute(model)
  outSub <- substitute(output)

  # Some checks. At least one new module. 'workflow' is from zoon workflow call.
  unchanged <- vapply(list(occSub, covSub, proSub, modSub, outSub),
                      is.null, FUN.VALUE = FALSE)
  if (all(unchanged))
    stop("At least one module type must be changed.")
  
  stopifnot(inherits(workflow, "zoonWorkflow"))

  # Separate the original work flow.
  oldCallArgs <- SplitCall(workflow$call)
  # Convert strings to calls
  oldCallArgs <- lapply(oldCallArgs, StringToCall)

  # Replace any arguments that have been specified.
  if (!is.null(occSub))
    oldCallArgs[["occurrence"]] <- occSub

  if (!is.null(covSub))
    oldCallArgs[["covariate"]] <- covSub

  if (!is.null(proSub))
    oldCallArgs[["process"]] <- proSub

  if (!is.null(modSub))
    oldCallArgs[["model"]] <- modSub

  if (!is.null(outSub))
    oldCallArgs[["output"]] <- outSub

  if (!is.null(forceReproducible))
    oldCallArgs$forceReproducible <- forceReproducible

  # Work out where to run the workflow from.
  from <- which.max(!unchanged)

  # Give new arg names to *Sub objects so we can continue with RerunWorkflow
  # source code.
  occNew <- oldCallArgs[["occurrence"]]
  covNew <- oldCallArgs[["covariate"]]
  proNew <- oldCallArgs[["process"]]
  modNew <- oldCallArgs[["model"]]
  outNew <- oldCallArgs[["output"]]
  forceReproducible <- as.logical(oldCallArgs[["forceReproducible"]])

  #####
  # From here is the same as RerunWorkflow.

  # save the local environment as it needs to be passed to various functions.
  e <- new.env(parent = parent.frame())

  # capture the session info to return in workflow object
  session.info <- sessionInfo()

  # Check all modules are of same list structure
  occurrence.module <- CheckModList(occNew)
  covariate.module <- CheckModList(covNew)
  process.module <- CheckModList(proNew)
  model.module <- CheckModList(modNew)
  output.module <- CheckModList(outNew)

  # create a list of these things to return
  call.list <- list(
    occurrence.module,
    covariate.module,
    process.module,
    model.module,
    output.module
  )

  # Only one of occurrence, covariate, process and model can be a list of
  #   multiple modules.
  
  isChain <- vapply(call.list,
                    function(x) {
                      isTRUE(attr(x, "chain"))
                    },
                    FUN.VALUE = FALSE)
  
  NoOfModules <- vapply(call.list, length, FUN.VALUE = 0)
  
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

  # Build module version list
  fun_ver <- function(x) c(module = x$func, version = x$version)
  eg <- c(module = "a", version = "b")
  
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

  # First the data collection modules
  # Actually tryCatch here only tells user which module broke, nothing to save.

  # set up zoon object now so we can return it if there's an error
  call <- SortArgs(
    PasteAndDep(occNew),
    PasteAndDep(covNew),
    PasteAndDep(proNew),
    PasteAndDep(modNew),
    PasteAndDep(outNew),
    forceReproducible
  )

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
  on.exit(return(output))


  if (from <= 1) {
    tryCatch(
      {
        occurrence.output <- lapply(occurrenceName, FUN = DoOccurrenceModule, e)
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
        process.output <- DoProcessModules(process.module,
                                           processName,
                                           data,
                                           e)
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
        output.output <- DoOutputModules(output.module,
                                         outputName,
                                         process.module,
                                         process.output,
                                         model.output,
                                         e)
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
