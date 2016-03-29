#' @title Process module: OneHundredBackground
#'
#' @description Process module to generate up to 100 background records at random in
#'      cells of ras and return these along with the presence only data.
#'
#' @param .data \strong{Internal parameter, do not use in the workflow function}. \code{.data} is a list of a data frame and a raster object returned from occurrence modules and covariate modules respectively. \code{.data} is passed automatically in workflow from the occurrence and covariate modules to the process module(s) and should not be passed by the user.
#' @param seed Numeric used with \code{\link[base]{set.seed}}
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#'
#' @section Data type: presence-only
#'
#' @name OneHundredBackground
#' @family process
OneHundredBackground <- function (.data, seed = NULL) {
  
  zoon:::GetPackage(dismo)
  
  occurrence <- .data$df
  ras <- .data$ras
 
  if (!all(occurrence$type == 'presence')) {
    stop ('"OneHundredBackground" module only works for presence-only data')
  }
  
  # set seed if specified
  if(!is.null(seed)){
    if(inherits(x = seed, what = c('numeric', 'integer'))){
      set.seed(seed)
    } else {
      stop("'seed' must be numeric or NULL")
    }
  }
  
  # generate pseudo-absence data
  points <- 100
  if(ncell(ras) < 100){
    points <- ncell(ras)
    warning(paste0('There are fewer than 100 cells in the environmental raster.', 
      '\nUsing all available cells (', ncell(ras), ') instead'))
  }
  pa <- randomPoints(ras, points)
  
  npres <- nrow(occurrence)
  
  npabs <- nrow(pa)
  
  # extract covariates from raster
  occ_covs <- as.matrix(extract(ras, occurrence[, c('longitude', 'latitude')]))
  
  pa_covs <- as.matrix(extract(ras, pa))
  
  # write site level covariates 
  site.covs <- as.data.frame(rbind(occ_covs, pa_covs))
  names(site.covs) <- names(.data$site.covariates)
   
  # write NA values for observation level covariates
  obs.covs <- as.data.frame(matrix(NA,
                                   ncol = length(names(.data$obs.covariates)),
                                   nrow = length(pa[,1]) + nrow(site.covs)))
  names(obs.covs) <- names(.data$obs.covariates)
   
  # write occurrence data
  df <- data.frame(value = rep(c(1, 0),
                               c(npres, npabs)),
                   type = rep(c('presence', 'background'),
                              c(npres, npabs)),
                   fold = rep(1, npres + npabs),
                   longitude = c(occurrence$lon, pa[, 1]),
                   latitude = c(occurrence$lat, pa[, 2]))
  
  # Can allow for less than 100 points to return
  # after NA removal?
  
  # remove NA values
  if(NROW(na.omit(site.covs)) > 0){
    idx <- which(!rowSums(is.na(site.covs)) > 0)
    df <- df[idx,]
    site.covs <- site.covs[idx,]
    obs.covs <- obs.covs[idx,]
  }
  
  return(list(df=df, ras=ras, site.covariates = site.covs, obs.covariates = obs.covs))
  
}

