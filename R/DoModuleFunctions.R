
# Code to run all output modules
# If not chained, we need to apply over output modules, 
#   covariate output or model output.
# If chained, we certainly need to apply over output modules,
#   AND apply over covariate or model output.

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

    # Otherwise model may be parallel. If not, this will still run the 
    #   single model ok.
    } else {
      output.output <- lapply(model.output, 
                         function(x) do.call(outputName[[1]]$func, 
                         c(list(.model = x,
                                .ras = process.output[[1]]$ras),
                           outputName[[1]]$paras), envir = e))
    }
  # Chained
  } else {
      output.output <- lapply(model.output, 
                         function(y) lapply(outputName, 
                           function(x) do.call(x$func, 
                           c(list(.model = y,
                                  .ras = process.output[[1]]$ras),
                             x$paras), envir = e)))    
    
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

