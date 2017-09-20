#' Replicate a module multiple times
#'
#' This function is useful when running simulations and could be used
#' with modules that have random number generation internally meaning that
#' results from identical runs are different. Replicate gives the same result
#' as using list() and repeating the module multiple times.
#'
#' @param call A module call, e.g. UKAnophelesPlumbeus
#' @param n The number of times to replicate the call
#'
#' @return A list of calls
#' @name Replicate
#' @export
#' @examples
#' # run a workflow, using the logistic regression model
#' \dontrun{
#'
#' Without Replicate
#' work1 <- workflow(occurrence = list(UKAnophelesPlumbeus,UKAnophelesPlumbeus,UKAnophelesPlumbeus),
#'                covariate = UKAir,
#'                process = OneHundredBackground,
#'                model = LogisticRegression,
#'                output = SameTimePlaceMap)
#' # With Replicate
#' work2 <- workflow(occurrence = Replicate(UKAnophelesPlumbeus, 3),
#'                covariate = UKAir,
#'                process = OneHundredBackground,
#'                model = LogisticRegression,
#'                output = SameTimePlaceMap)
#' # The workflows are the same
#' plot(work1)
#' plot(work2)
#'
#' # Output plots show the random placement of background points
#' # in each run
#' work1 <- workflow(occurrence = UKAnophelesPlumbeus,
#'                  covariate = UKAir,
#'                  process = Replicate(Background(n=10), n = 10),
#'                  model = LogisticRegression,
#'                  output = PrintMap)
#'
#' }

Replicate <- function(call, n) {
  mod_call <- substitute(call)
  rep_list <- list(mod_call)
  return(rep_len(rep_list, n))
}
