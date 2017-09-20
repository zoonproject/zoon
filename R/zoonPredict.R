# functions for handling prediction from model module outputs

#' ZoonPredict
#'
#' \strong{module developer tool:} Predict from a ZoonModel object
#'
#' @details \strong{This function is only intended to be used when developing
#' new modules, not for running zoon workflows}.
#' Given a \code{zoonModel} object returned by a model
#' module using the function \code{ZoonModel}, make a prediction to
#' a new dataframe. Values returned must be on the response scale
#' (e.g. probabilities of presence).
#' For an example, see the source code for the module \code{mgcv}.
#'
#'
#'
#' @param zoonModel a \code{zoonModel} object
#'
#' @param newdata a dataframe containing data to predict to.
#' @name ZoonPredict
#' @export

ZoonPredict <- function(zoonModel, newdata) {

  # check the model
  if (!inherits(zoonModel, "zoonModel")) {
    stop("zoonPredict can only be used with zoonModel objects")
  }

  # get required packages
  if (!is.null(zoonModel$packages)) {
    require (
      zoonModel$packages,
      character.only = TRUE
    )
  }


  # define prediction function using module code
  fun_text <- sprintf(
    "fun <- function (model, newdata) {%s}",
    zoonModel$code
  )
  fun <- eval(parse(text = fun_text))

  # run the predictor and return result
  ans <- fun(zoonModel$model, newdata = newdata)
  return(ans)
}


#' ZoonModel
#'
#' \strong{module developer tool:} Create a Zoon model object
#'
#' @details \strong{This function is only intended to be used when developing
#' new modules, not for running zoon workflows}.
#' Given a \code{zoonModel} object returned by a model
#' module using the function \code{ZoonModel}, make a prediction to
#' a new dataframe.
#' For an example, see the source code for the module \code{InteractiveMap}.
#'
#' @param model a fitted model object to be used for making predictions
#'
#' @param code code to make predictions from \code{model} object to
#' a dataframe \code{newdata} containing new covariate observations.
#' The code must use the objects named \code{model} and \code{newdata} and
#' no other objects and must return a numeric vector, with the same length
#' as the number of rows in \code{newdata} giving predictions on the response
#' scale (e.g. probabilities of presence).
#'
#' @param packages a character vector giving the names of packages
#' needed to run the code zoonModel a \code{zoonModel} object
#'
#' @return an object of class \code{zoonModel} containing all of the
#'  information and code required to make predictions, using the function
#'  \code{\link{ZoonPredict}}
#'
#' @name ZoonModel
#' @export
# @family module developer tools
ZoonModel <- function(model,
                      code,
                      packages) {

  # catch the code as text
  code <- deparse(substitute(code))
  code <- paste(code, collapse = "\n")

  # create a list of these elements
  ans <- list(
    model = model,
    code = code,
    packages = packages
  )

  class(ans) <- "zoonModel"

  return(ans)
}
