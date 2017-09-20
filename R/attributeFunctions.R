############################################################################################
## Functions for manipulating data.frame objects while preserving user specified attributes##
############################################################################################

#' cbindZoon
#'
# The user defined attributes from data.frame a will be preserved cbind'ing data.frame b.
#'
#' @param a The dataframe of which you'd like preserve the user defined attributes
#' @param b The dataframe which you'd like to append to a.
#' @name cbindZoon
#' @export
cbindZoon <- function(a, b) {
  attr.list <- attributes(a) # Extract attribute list
  appended.frame <- cbind(a, b) # append dataframe
  attr.list$names <- attr(appended.frame, which = "names") # update names in attribute list
  attributes(appended.frame) <- attr.list # set attributes for appended dataframe
  return(appended.frame)
}

#' subsetColumnsZoon
#'
#' Extract a subset of columns from a data.frame, while preserving the user
#' defined attributes of the parent data.frame.
#'
#' @param df The dataframe of which you'd like to subset columns from
#' @param columns A vector of column names (character) of indexs (numeric) which you'd like to keep
#' @name subsetColumnsZoon
#' @export
subsetColumnsZoon <- function(df, columns) {
  attr.list <- attributes(df) # Extract attribute list
  output <- df[, columns, drop = FALSE] # subset dataframe

  attr.list$names <- attr(output, which = "names") # update names in attribute list
  attributes(output) <- attr.list # set attributes for appended dataframe

  return(output)
}
