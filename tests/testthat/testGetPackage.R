context('GetPackage')

# This is interactive and so is hard to test

test_that('GetPackage', {
  
  t <- 3
  expect_error(zoon::GetPackage(t), 'package must be a character')
  
  rncep <- try(zoon::GetPackage('lme4'), silent = TRUE)
  expect_null(rncep, info = 'GetPackage has failed with "lme4"')
  
})