context('testModules')

test_that('repo modules pass', {
  
  skip_on_cran()
  
  # Output
  expect_true(x <- zoon:::test_module('https://raw.githubusercontent.com/zoonproject/modules/master/R/PrintMap.R'))
  
  # Process PO
  expect_true(x <- zoon:::test_module('https://raw.githubusercontent.com/zoonproject/modules/master/R/OneHundredBackground.R'))
  
  # Process PA
  expect_true(x <- zoon:::test_module('https://raw.githubusercontent.com/zoonproject/modules/master/R/Crossvalidate.R'))
  
  # Model PA
  expect_true(x <- zoon:::test_module('https://raw.githubusercontent.com/zoonproject/modules/master/R/LogisticRegression.R'))
  
  # Model PB
  expect_true(x <- zoon:::test_module('https://raw.githubusercontent.com/zoonproject/modules/master/R/MaxLike.R'))
  
  # Covariate
  expect_true(x <- zoon:::test_module('https://raw.githubusercontent.com/zoonproject/modules/master/R/UKAir.R'))
  
  # Occurrence PO
  expect_true(x <- zoon:::test_module('https://raw.githubusercontent.com/zoonproject/modules/master/R/AnophelesPlumbeus.R'))
  
  # Occurrence PA
  expect_true(x <- zoon:::test_module('https://raw.githubusercontent.com/zoonproject/modules/master/R/CWBZimbabwe.R'))
  
})

