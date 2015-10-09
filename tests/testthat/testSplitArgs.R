context('SplitArgs')

test_that('SplitArgs works', {
  #' A function that takes a string (from workflow$call) and splits it into a
  #'   module name and it's arguments.
  #'
  #'@param string A string of the form "moduleName" or 
  #'  "moduleName(parameter = 2, parameter2 = 3)"
  
  a <- SplitArgs("moduleName(parameter = 2, parameter2 = 3)")
  b <- SplitArgs("moduleName")
  
  # Interesting to note that SplitArgs doesn't deal with numerics at all. 
  #   Probably needs looking into.
  expect_equal(a, list(module = 'moduleName', paras = list(parameter = '2', parameter2 = '3')))
  expect_equal(b, list(module = 'moduleName', paras = list()))
})