#' @title Output module: PerformanceMeasures
#'
#' @description Calculate a suite of performance metrics on either crossvalidation, external validation data or (at your own risk) in-sample validation.
#'
#' @details This model prints to console a number of metrics of the model fitted:
#' \itemize{
#'  \item \code{auc} - (see ?SDMTools::auc) The Area Under the Curve of the Receiver operating characteristic using a Mann-Whitney U statistic
#'  \item \code{\link{kappa}} - (see ?kappa) An estimate of the 2-norm condition number of a matrix or of the R matrix of a QR decomposition, perhaps of a linear fit. The 2-norm condition number can be shown to be the ratio of the largest to the smallest non-zero singular value of the matrix.
#'  \item \code{\link{omissions}} - (see ?SDMTools::omission) The ommission rate as a proportion of true occurrences misidentified given the defined threshold value.
#'  \item \code{sensitivity} - (see ?SDMTools::sensitivity) The proportion of actual presences predicted given the defined threshold value.
#'  \item \code{specificity} - (see ?SDMTools::specificity) The proportion of actual absences predicted given the defined threshold value.
#'  \item \code{proportionCorrect} - (see ?SDMTools::prop.correct) The proportion of the presence and absence records correctly identified given the defined threshold value.
#' }
#' 
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @param threshold A chosen threshold value for measures that need 0/1 predictions 
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#'
#' @name PerformanceMeasures
#' @family output
PerformanceMeasures <-
function(.model, .ras, threshold = 0.5){

  zoon:::GetPackage(SDMTools)

  if (all(.model$data$predictions %in% c(0,1))){
    warning('The model has predicted presence/absence rather than probabilities. Some measures may not work')
  }

  if (all(.model$data$fold == 1)){
    warning('You have no cross-validation folds, validation statistics may be misleading')

    # make predictions for the model

    p <- ZoonPredict(zoonModel = .model$model,
                     newdata = list(site.coefficients = .model$site.coeffifients,
                                    obs.coefficients = .model$obs.coeffifients))
    confusion <- SDMTools::confusion.matrix(.model$data$value,
                                            p,
                                            threshold)
    
    performance <- list(
      auc = SDMTools::auc(.model$data$value, p),
      kappa = Kappa(confusion),
      omissions = omission(confusion),
      sensitivity = sensitivity(confusion),
      specificity = specificity(confusion),
      proportionCorrect = prop.correct(confusion)
    )
  } else if (all(.model$data$fold >= 1)){
    
    confusion <- SDMTools::confusion.matrix(.model$data$value, .model$data$predictions)

    performance <- list(
      auc = SDMTools::auc(.model$data$value, .model$data$predictions),
      kappa = Kappa(confusion),
      omissions = omission(confusion),
      sensitivity = sensitivity(confusion),
      specificity = specificity(confusion),
      proportionCorrect = prop.correct(confusion)
    )
    
  } else if (all(.model$data$fold %in% c(0,1))){

    data <- .model$data[.model$data$fold == 0,]
    confusion <- SDMTools::confusion.matrix(data$value, data$predictions, threshold)

    performance <- list(
      auc = SDMTools::auc(data$value, data$predictions),
      kappa = Kappa(confusion),
      omissions = omission(confusion),
      sensitivity = sensitivity(confusion),
      specificity = specificity(confusion),
      proportionCorrect = prop.correct(confusion)
    )
  }
  
  message('Model performance measures:')
  for(i in 1:length(performance)) {
    line <- paste0(names(performance)[i],
                   ' :  ',
                   performance[i])
    message(line)
  }
  message(' ')

  return (performance)

}
