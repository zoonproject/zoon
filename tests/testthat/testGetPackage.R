context('GetPackage')

# This is interactive and so is hard to test

test_that('GetPackage', {
  
  t <- 3
  expect_error(zoon::GetPackage(t), 'package must be a character')
  
})