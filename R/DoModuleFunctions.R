
# Code to run all output modules
# If not chained, we need to apply over output modules,
#   covariate output or model output.
# If chained, we certainly need to apply over output modules,
#   AND apply over covariate or model output.

# Options are:
#   Output listed.
#   Model listed (therefore model will have more outputs than process)
#   Process, covariate or occurrence listed (model will have same number of
#     outputs as process and list elements need to be matched.
#
#  Output Chained & model listed
#  Output Chained & process, covariate or occurrence listed

# FLOW:
# Not Chained
#   length(output) > 1 (output is a list)
#   length(output) == 1 (output isn't a list)
#     length(process) == length(model) (proc, cov or occ is a list)
#     length(process) != length(model) (model or nothing is a list)
#
# Is Chained
#   length(process) == length(model) (proc, cov or occ is a list)
#   length(process) != length(model) (model or nothing is a list)


DoOutputModules <- function(output.module, outputName, process.module,
                            process.output, model.output, e) {
  DoOutputList <- function(x, model.output, process.output, e) {
    output.output <- do.call(
      x$func,
      c(
        list(
          .model = model.output[[1]],
          .ras = process.output[[1]]$ras
        ),
        x$paras
      ), envir = e
    )

    if (!is.null(output.output)) {
      attr(output.output, "call_path") <- c(
        attr(model.output[[1]], "call_path"),
        output = as.character(x$module)
      )
    }

    return(output.output)
  }

  DoOutputPairedList <- function(x, outputName, e) {
    output.output <- do.call(
      outputName[[1]]$func,
      c(
        list(
          .model = x[[1]],
          .ras = x[[2]]$ras
        ),
        outputName[[1]]$paras
      ),
      envir = e
    )

    if (!is.null(output.output)) {
      attr(output.output, "call_path") <- c(
        attr(x[[1]], "call_path"),
        output = as.character(outputName[[1]]$module)
      )
    }

    return(output.output)
  }

  DoOutputModelList <- function(x, outputName, process.output, e) {
    output.output <- do.call(
      outputName[[1]]$func,
      c(
        list(
          .model = x,
          .ras = process.output[[1]]$ras
        ),
        outputName[[1]]$paras
      ),
      envir = e
    )

    if (!is.null(output.output)) {
      attr(output.output, "call_path") <- c(
        attr(x, "call_path"),
        output = as.character(outputName[[1]]$module)
      )
    }

    return(output.output)
  }

  # Not chained
  if (!identical(attr(output.module, "chain"), TRUE)) {
    # if output is a list
    if (length(output.module) > 1) {

      # There must be only one model and process
      output.output <- lapply(
        outputName, FUN = DoOutputList, e = e,
        model.output = model.output,
        process.output = process.output
      )

      # Otherwise model may be parallel. If not, this will still run the
      #   single model ok.
    } else {
      # Multiple models a result of list in Occ or Cov modules
      if (length(process.output) == length(model.output)) {

        # Create a paired list of model and process output
        MP.output <- list()

        for (i in seq_along(process.output)) {
          MP.output[[i]] <- list(model.output[[i]], process.output[[i]])
        }

        output.output <- lapply(
          MP.output, FUN = DoOutputPairedList,
          e = e, outputName = outputName
        )
      } else { # there must be only one process output and multiple models

        output.output <- lapply(
          model.output,
          FUN = DoOutputModelList,
          e = e,
          process.output = process.output,
          outputName = outputName
        )
      }
    }
    # Chained
  } else {

    #   Process, covariate or occurrence listed (model will have same number of
    #     outputs as process and list elements need to be matched.
    if (length(process.output) == length(model.output)) {

      # Create a paired list of model and process output
      MP.output <- list()

      for (i in seq_along(process.output)) {
        MP.output[[i]] <- list(model.output[[i]], process.output[[i]])
      }

      output.output <- lapply(
        MP.output,
        function(x) lapply(
          outputName,
          function(y) {
            output.output <- DoOutputPairedList(
              x = x,
              outputName = list(y),
              e = e
            )
          }
        )
      )
    } else { # there must be only one process output and multiple models

      output.output <- unlist(lapply(
        model.output,
        function(y) lapply(
          outputName,
          function(x) {
            output.output <- DoOutputList(
              x = x,
              model.output = list(y),
              process.output = process.output,
              e = e
            )
          }
        )
      ), recursive = FALSE)
    }
  }
  return(output.output)
}




# Do all model modules
# If listed model modules, we need to apply over models
#   otherwise need to apply over process output.
# We put model function names into RunModels which runs models multiple
#   times as requested by cross validation/external validation
#   and predicts all test data.

DoModelModules <- function(model.module, modelName, process.output, e) {
  DoModelList <- function(x, e, process.output) {
    model.output <- do.call(
      RunModels,
      list(
        df = process.output[[1]]$df,
        modelFunction = x$func,
        paras = x$paras,
        workEnv = e
      ),
      envir = e
    )

    call_path <- c(
      attr(process.output[[1]], "call_path"),
      model = as.character(x$module)
    )

    attr(model.output, "call_path") <- call_path

    return(model.output)
  }

  DoModelNotList <- function(x, e, modelName) {
    model.output <- do.call(
      RunModels,
      list(
        df = x$df,
        modelFunction = modelName[[1]]$func,
        paras = modelName[[1]]$paras,
        workEnv = e
      ),
      envir = e
    )

    call_path <- c(
      attr(x, "call_path"),
      model = as.character(modelName[[1]]$module)
    )

    attr(model.output, "call_path") <- call_path

    return(model.output)
  }

  if (length(model.module) > 1) {
    model.output <- lapply(modelName,
                           FUN = DoModelList,
                           e = e,
                           process.output = process.output)
  } else {
    model.output <- lapply(process.output,
                           FUN = DoModelNotList,
                           e = e,
                           modelName = modelName)
  }
  return(model.output)
}




# Do all process modules
# If process is not chained, then either apply over process (for list of
#   process modules) or over 'data' which is list combining data from covariate
#   and occurrence modules.
# If process IS chained, then we loop through process modules putting output of
#   one as input to next.
DoProcessModules <- function(process.module, processName, data, e) {
  DoProcessList <- function(x, e) {
    process.output <- do.call(
      x$func,
      c(list(.data = data[[1]]), x$paras),
      envir = e
    )
    call_path <- list(
      occurrence = attr(data[[1]]$df, "call_path")$occurrence,
      covariate = attr(data[[1]]$ras, "call_path")$covariate,
      process = as.character(x$module)
    )
    attr(process.output, "call_path") <- call_path

    return(process.output)
  }

  DoProcessNotList <- function(x, e) {
    process.output <- do.call(
      processName[[1]]$func,
      c(list(.data = x), processName[[1]]$paras),
      envir = e
    )
    call_path <- list(
      occurrence = attr(x$df, "call_path")$occurrence,
      covariate = attr(x$ras, "call_path")$covariate,
      process = as.character(processName[[1]]$module)
    )
    attr(process.output, "call_path") <- call_path

    return(process.output)
  }

  DoProcessChain <- function(x, p, e) {
    process.output <- do.call(
      processName[[p]]$func,
      c(list(.data = x), processName[[p]]$paras),
      envir = e
    )
    call_path <- list(
      occurrence = attr(x$df, "call_path")$occurrence,
      covariate = attr(x$ras, "call_path")$covariate
    )
    attr(process.output, "call_path") <- call_path

    return(process.output)
  }

  if (!identical(attr(process.module, "chain"), TRUE)) {
    if (length(processName) > 1) {
      process.output <- lapply(processName, FUN = DoProcessList, e = e)
    } else {
      process.output <- lapply(data, FUN = DoProcessNotList, e = e)
    }
  } else {
    # If process was chained, we must loop through the process modules
    #   applying them to the output of the previous one.
    # If covariate or occurrence was list, data will be list w/ length > 1
    #   so we assign data -> process.output then apply over that.

    # We might want to save output of each process and return all of them
    #   at the end of the workflow.
    process.output <- data
    for (p in seq_along(processName)) {
      process.output <- lapply(process.output,
                               FUN = DoProcessChain,
                               p = p,
                               e = e)
    }

    call_path_process_chain <- function(x) {
      attr(x, "call_path") <- c(
        attr(x, "call_path"),
        process = paste(
          "Chain(",
          paste(
            lapply(processName, function(x) x$module),
            collapse = ", "
          ),
          ")", sep = ""
        )
      )
      return(x)
    }

    process.output <- lapply(process.output, call_path_process_chain)

  }
  return(process.output)
}


# DoOccurrenceModule is for occurrence modules
# and is very simple. x is the name of the module
# (occurrenceName)
DoOccurrenceModule <- function(x, e) {
  occurrence.output <- do.call(x$func, x$paras, envir = e)
  call_path <- list(occurrence = as.character(x$module))
  attr(occurrence.output, "call_path") <- call_path
  return(occurrence.output)
}

# DoCovariateModule is for covariate modules
# and is very simple. x is the name of the module
# (covariateName)
DoCovariateModule <- function(x, e) {
  covariate.output <- do.call(x$func, x$paras, envir = e)
  call_path <- list(covariate = as.character(x$module))
  attr(covariate.output, "call_path") <- call_path
  return(covariate.output)
}
