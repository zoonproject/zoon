#' @title Process module: Crossvalidate
#'
#' @description Run k fold crossvalidation. If presence absence, split presences and absences separately so folds have equally balanced data. Otherwise just sample.
#'
#' @param .data \strong{Internal parameter, do not use in the workflow function}. \code{.data} is a list of a data frame and a raster object returned from occurrence modules and covariate modules respectively. \code{.data} is passed automatically in workflow from the occurrence and covariate modules to the process module(s) and should not be passed by the user.
#'
#' @param k Positive integer number of folds to split the data into. Default is 5.
#'  
#' @param seed Numeric used with \code{\link[base]{set.seed}}
#' 
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'
#' @name Crossvalidate
#' @family process
Crossvalidate <-
function (.data, k = 5, seed = NULL) {
  
  ## Why are the raster covariates extracted here again?  
  ## I removed this, as the function ExtractAndCombineData
  ## already performs this.
  
  occurrence <- .data$df
  
  if (all(occurrence$value == 1)) {
    warning ('You currently only have presence points. Unless you are using presence only modelling, create some pseudoabsence points before this module.')
  }
  
  # set seed if specified
  if(!is.null(seed)){
    if(inherits(x = seed, what = c('numeric', 'integer'))){
      set.seed(seed)
    } else {
      stop("'seed' must be numeric or NULL")
    }
  }
  
  # if presence absence, create folds separately to give well balanced groups
  # if presence only or abundance etc., just split randomly into folds.
  if (all(c(0,1) %in% occurrence$value) & all(occurrence$value %in% c(0,1) )){
    fold <- rep(NA, NROW(occurrence))
    fold[occurrence$value == 1] <- sample(1:k, sum(occurrence$value == 1), replace=TRUE)
    fold[occurrence$value == 0] <- sample(1:k, sum(occurrence$value == 0), replace=TRUE)
  } else {
    fold <- sample(1:k, NROW(occurrence), replace=TRUE)
  }

  # combine with the occurrence data
  df <- data.frame(value = occurrence$value,
                   type = occurrence$type,
                   fold = fold,
                   longitude = occurrence$lon,
                   latitude = occurrence$lat)

  return(list(df=df, ras=.data$ras, site.covariates = .data$site.covariates, obs.covariates = .data$osb.covariates))
}
