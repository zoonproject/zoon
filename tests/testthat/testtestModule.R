context('testModules')

test_that('repo modules pass', {
  
  skip_on_cran()
  
  # Output
  expect_true(x <- zoon:::test_module('https://raw.githubusercontent.com/zoonproject/modules/master/R/PrintMap.R'))
  
  # Process
  expect_true(x <- zoon:::test_module('https://raw.githubusercontent.com/zoonproject/modules/master/R/OneHundredBackground.R'))
  
  # Model
  expect_true(x <- zoon:::test_module('https://raw.githubusercontent.com/zoonproject/modules/master/R/LogisticRegression.R'))
  
  # Covariate
  expect_true(x <- zoon:::test_module('https://raw.githubusercontent.com/zoonproject/modules/master/R/UKAir.R'))
  
  # Occurrence
  expect_true(x <- zoon:::test_module('https://raw.githubusercontent.com/zoonproject/modules/master/R/AnophelesPlumbeus.R'))
  
})

