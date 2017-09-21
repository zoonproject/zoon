#' Accessor functions for getting module outputs from a workflow object
#'
#' These functions access the output from each module type. If workflows
#'   are split using list, they will return a list with the output of
#'   each separate workflow being one element of the list.
#'
#' @param workflow A workflow object
#'
#' @name Occurrence
#'
#' @export
#'
#' @examples
#' \dontrun{
#' work1 <- workflow(occurrence = UKAnophelesPlumbeus,
#'                  covariate  = UKAir,
#'                  process    = Background(n = 70),
#'                  model      = list(LogisticRegression, LogisticRegression),
#'                  output     = PrintMap)
#'
#' Occurrence(work1)
#' Covariate(work1)
#' Process(work1)
#' Model(work1)
#' Model(work1)[[1]]
#' Output(work1)
#' }

Occurrence <- function(workflow) {
  
  if (!inherits(workflow, "zoonWorkflow"))
    stop("workflow should be a zoon workflow object.")

  if (length(workflow$occurrence.output) == 1)
    out <- workflow$occurrence.output[[1]]
  else
    out <- workflow$occurrence.output

  out
}



#' @rdname Occurrence
#' @name Covariate
#' @export

Covariate <- function(workflow) {
  if (!inherits(workflow, "zoonWorkflow"))
    stop("workflow should be a zoon workflow object.")

  if (length(workflow$covariate.output) == 1)
    out <- workflow$covariate.output[[1]]
  else
    out <- workflow$covariate.output

  out
}



#' @rdname Occurrence
#' @name Process
#' @export

Process <- function(workflow) {
  if (!inherits(workflow, "zoonWorkflow"))
    stop("workflow should be a zoon workflow object.")

  if (length(workflow$process.output) == 1)
    out <- workflow$process.output[[1]]
  else
    out <- workflow$process.output

  out
}

#' @rdname Occurrence
#' @name Model
#' @export
Model <- function(workflow) {
  if (!inherits(workflow, "zoonWorkflow"))
    stop("workflow should be a zoon workflow object.")

  if (length(workflow$model.output) == 1)
    out <- workflow$model.output[[1]]
  else
    out <- workflow$model.output

  out
}


#' @rdname Occurrence
#' @name Output
#' @export
Output <- function(workflow) {
  if (!inherits(workflow, "zoonWorkflow"))
    stop("workflow should be a zoon workflow object.")

  if (length(workflow$report) == 1)
    out <- workflow$report[[1]]
  else
    out <- workflow$report

  out
}
