# A function for testing parameters (within a context)
test_parameters <- function(roxy_parse, defaultParams = NULL, modulePath) {

  # Extract the parameters
  params <- formals(source(modulePath)$value)

  # Create blank symbol to test against
  blank <- formals(function(blank) {
  })

  test_that(paste("Check parameter names for", basename(modulePath)), {

    # Extract names from tags
    paramNames <- unlist(lapply(
      roxy_parse[names(roxy_parse) == "param"],
      function(x) x$name
    ))

    # Check for defult parameter
    if (!is.null(defaultParams)) expect_true(
      all(defaultParams %in% paramNames),
      info = paste("Default parameter(s) is not documented:",
                   paste(defaultParams[!defaultParams %in% paramNames],
                         collapse = ", "))
    )

    # Check all parameters are documented
    expect_true(
      all(names(params) %in% paramNames),
      info = paste(
        "Parameter(s) are missing documentation:",
        paste(names(params)[!names(params) %in% paramNames], collapse = ", ")
      )
    )

    # Check all documented parameters exist
    expect_true(
      all(paramNames %in% names(params)),
      info = paste(
        "Documented parameters do not exist:",
        paste(paramNames[!paramNames %in% names(params)], collapse = ", ")
      )
    )
  })

  test_that(paste("Check default values for", basename(modulePath)), {

    # Expect that all non-default parameters have defaults
    paramClasses <- lapply(params, function(x) identical(x, blank$blank))

    # Remove ellipsis
    paramClasses <- paramClasses[!names(paramClasses) %in% "..."]

    # remove defaults
    if (!is.null(defaultParams))
      paramClasses <- paramClasses[!names(paramClasses) %in% defaultParams]

    expect_true(
      !any(unlist(paramClasses)),
      info = paste("Some parameters are missing default values:",
                   names(unlist(paramClasses)[unlist(paramClasses)]))
    )
  })
}
