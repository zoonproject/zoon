
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
#     length(process) == length(model) (process, covariate or occurrence is a list)
#     length(process) != length(model) (model or nothing is a list)
#
# Is Chained
#   length(process) == length(model) (process, covariate or occurrence is a list)
#   length(process) != length(model) (model or nothing is a list)


DoOutputModules <- function(output.module, outputName, process.module, 
                     process.output, model.output, e) {
  # Not chained
  if(!identical(attr(output.module, 'chain'), TRUE)){
    # if output is a list
    if (length(output.module) > 1){
      output.output <- lapply(outputName, 
                         function(x) do.call(x$func, 
                         c(list(.model = model.output[[1]], 
                                .ras = process.output[[1]]$ras),
                           x$paras), envir = e))

    } else {
      
      #   Process, covariate or occurrence listed (model will have same number of
      #     outputs as process and list elements need to be matched.
      if (length(process.output) == length(model.output)){

        # Create a paired list of model and process output
        MP.output <- list()

        for(i in 1:length(process.output)){
          MP.output[[i]] <- list(model.output[[i]], process.output[[i]])
        }

        output.output <- lapply(MP.output,
                                function(x) do.call(outputName[[1]]$func, 
                                                    c(list(.model = x[[1]],
                                                           .ras = x[[2]]$ras),
                                                      outputName[[1]]$paras), envir = e))


      } else { # there must be only one process output and multiple models

          output.output <- lapply(model.output,
                                  function(x) do.call(outputName[[1]]$func, 
                                                      c(list(.model = x,
                                                             .ras = process.output[[1]]$ras),
                                                        outputName[[1]]$paras), envir = e))

      }

    }
  # Chained
  } else {

      #   Process, covariate or occurrence listed (model will have same number of
      #     outputs as process and list elements need to be matched.
      if (length(process.output) == length(model.output)){

        # Create a paired list of model and process output
        MP.output <- list()

        for(i in 1:length(process.output)){
          MP.output[[i]] <- list(model.output[[i]], process.output[[i]])
        }

        output.output <- lapply(MP.output, 
                           function(x) lapply(outputName, 
                             function(y) do.call(y$func, 
                             c(list(.model = x[[1]],
                                    .ras = x[[2]]$ras),
                               y$paras), envir = e)))    


      } else { # there must be only one process output and multiple models

        output.output <- lapply(model.output, 
                           function(y) lapply(outputName, 
                             function(x) do.call(x$func, 
                             c(list(.model = y,
                                    .ras = process.output[[1]]$ras),
                               x$paras), envir = e)))    

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

DoModelModules <- function(model.module, modelName, process.output, e){

  if (length(model.module) > 1){
      model.output <- 
        lapply(modelName, 
               function(x) 
                 do.call(RunModels,
                         list(df = process.output[[1]]$df, 
                              modelFunction = x$func, 
                              paras = x$paras, 
                              workEnv = e
                             ),
                          envir = e
                        )
              )
    } else {
      model.output <- 
        lapply(process.output,
               function(x) 
                 do.call(RunModels, 
                         list(df = x$df, 
                              modelFunction = modelName[[1]]$func, 
                              paras = modelName[[1]]$paras, 
                              workEnv = e
                             ),
                         envir = e
                        )
              )
    }
  return(model.output)
}




# Do all process modules
# If process is not chained, then either apply over process (for list of 
#   process modules) or over 'data' which is list combining data from covariate
#   and occurrence modules.
# If process IS chained, then we loop through process modules putting output of
#   one as input to next.
DoProcessModules <- function(process.module, processName, data, e){

  if (!identical(attr(process.module, 'chain'), TRUE)){
    if (length(processName) > 1){
      process.output <- lapply(processName, 
        function(x) do.call(x$func, 
                      c(list(.data = data[[1]]), x$paras), 
                      envir = e))
    } else {
      process.output <- lapply(data,
        function(x) do.call(processName[[1]]$func, 
                      c(list(.data = x), processName[[1]]$paras), 
                      envir = e))
    }
  } else { 
    # If process was chained, we must loop through the process modules 
    #   applying them to the output of the previous one.
    # If covariate or occurrence was list, data will be list w/ length > 1
    #   so we assign data -> process.output then apply over that.

    # We might want to save output of each process and return all of them
    #   at the end of the workflow.
    process.output <- data
    for(p in 1:length(processName)){      
      process.output <- lapply(process.output,
        function(x) do.call(processName[[p]]$func, 
                      c(list(.data = x), processName[[p]]$paras), 
                      envir = e))
    }
  }
  return(process.output)
}

