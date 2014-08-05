
# ~~~~~~~~~~~~
# workflow wrapper function


#' workflow wrapper function
#'
#'@param ext A numeric vector of length 4 giving the coordinates of the 
#'      rectangular region within which to carry out the analysis, in the 
#'      order: xmin, xmax, ymin, ymax.
#'@param occurrenceFn The name of the function (module) to be used to get occurence data
#'@param covariateFn  The name of the function (module) to be used to get covariate data
#'@param processFn The name of the function (module) to be used to process the data
#'@param modelFn The name of the SDM model function (module) to be used 
#'@param mapFn The name of the function (module) to be used to map output
#'
#'@return A list with the extent, the results of each module and a copy of the
#'       code used to execute the workflow (what's there now should be source-able
#'       though I'm sure there is a much neater approach than the one I took - the
#'       ultimate aim would be a much nicer way of enhancing reproducibility).
#'@export
#'@name workflow
#'@examples x <- 2+2

workflow <- function(ext,
                     occurrenceFn,
                     covariateFn,
                     processFn,
                     modelFn,
                     mapFn) {
  
  # small helper function to dump the code for a function to a text string
  getSource <- function (object) {
    paste(deparse(object), collapse = '\n')
  }
  
  occ <- occurrenceFn(ext)
  cov <- covariateFn(ext)
  df <- processFn(occ, cov)
  m <- modelFn(df)
  map <- mapFn(m, cov)
  
  # get the command used to call this function
  bits <- sys.call()
  call <- paste0(bits[1],
                 '(',
                 paste(bits[-1],
                       collapse = ', '),
                 ')')
  
  # sorry about this spaghetti...
  workflow <- paste0(paste(bits[-2], 
                           c(getSource(workflow),
                             getSource(occurrenceFn),
                             getSource(covariateFn),
                             getSource(processFn),
                             getSource(modelFn),
                             getSource(mapFn)),
                           sep = ' <- ',
                           collapse = '\n\n'),
                     paste0('\n\n',
                            bits[2],
                            ' <- c(',
                            paste(ext, collapse = ', '),
                            ')\n'),
                     '\nans <- ',
                     call,
                     collapse = '\n\n\n')
  
  return(list(extent = extent,
              occ = occ,
              cov = cov,
              df = df,
              m = m,
              map = map,
              workflow = workflow))
}


