# A function for testing parameters (within a context)
test_parameters <- function(roxy_parse, defaultParams = NULL, modulePath){
  
  # Extract the parameters
  params <- formals(source(modulePath)$value)
  
  # Create blank symbol to test against
  blank <- formals(function(blank){})
  
  test_that(paste('Check parameter names for', basename(modulePath)), {
    
    # Extract names from tags
    paramNames <- unlist(lapply(roxy_parse[names(roxy_parse) == 'param'],
                                function(x) x$name))
    
    # Check for defult parameter
    if(!is.null(defaultParams)) expect_true(all(defaultParams %in% paramNames),
                                            info = 'Default parameter is not documented')
    
    # Check all parameters are documented
    expect_true(all(names(params) %in% paramNames),
                info = 'Parameters are missing documentation')
    
  })
  
  test_that(paste('Check default values for', basename(modulePath)), {
    
    # Expect that all non-default parameters have defaults
    paramClasses <- lapply(params, function(x) identical(x, blank$blank))
    
    # Remove ellipsis
    paramClasses <- paramClasses[!names(paramClasses) %in% '...']
    
    # remove defaults
    if(!is.null(defaultParams)) paramClasses <- paramClasses[!names(paramClasses) %in% defaultParams]
    
    expect_true(!any(unlist(paramClasses)),
                info = paste('Some parameters are missing default values:', names(unlist(paramClasses)[unlist(paramClasses)]))
    )
  })
}