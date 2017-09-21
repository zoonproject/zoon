## Functions for manipulating data.frame objects while preserving user specified
## attributes
#' cbindZoon
#'
# The user defined attributes from data.frame a will be preserved cbind'ing
# data.frame b.
#'
#' @param a The dataframe of which you'd like preserve the user defined
#'   attributes
#' @param b The dataframe which you'd like to append to a.
#' @name cbindZoon
#' @export
cbindZoon <- function(a, b) {
  
  # Extract attribute list
  attr.list <- attributes(a)
  
  # append dataframe
  appended.frame <- cbind(a, b)
  
  # update names in attribute list
  attr.list$names <- attr(appended.frame, which = "names")
  
  # set attributes for appended dataframe
  attributes(appended.frame) <- attr.list
  
  return (appended.frame)
}

#' subsetColumnsZoon
#'
#' Extract a subset of columns from a data.frame, while preserving the user
#' defined attributes of the parent data.frame.
#'
#' @param df The dataframe of which you'd like to subset columns from
#' @param columns A vector of column names (character) of indices (numeric)
#'   which you'd like to keep
#' @name subsetColumnsZoon
#' @export
subsetColumnsZoon <- function (df, columns) {
  
  # Extract attribute list
  attr.list <- attributes(df)
  # subset dataframe
  output <- df[, columns, drop = FALSE]
  # update names in attribute list
  attr.list$names <- attr(output, which = "names")
  # set attributes for appended dataframe
  attributes(output) <- attr.list

  return (output)
  
}
