context('LapplyGetyModule')

test_that('LapplyGetModule works correctly', {
  
  a <- list(module = 'UKAir', paras = list())
  b <- list(module = 'UKAir', paras = list())
  
  c <- LapplyGetModule(list(a,b), forceReproducible=FALSE)
  
  cexpect <- list(list(module = 'UKAir', paras = list(), func = 'UKAir'), 
                  list(module = 'UKAir', paras = list(), func = 'UKAir'))
  
  expect_equal(c, cexpect)
  
  d <- list(module = 'UKAir', paras = list(a = 2, b = 'a'))
  
  e <- LapplyGetModule(list(a,d), forceReproducible=FALSE)
  
  eexpect <- list(list(module = 'UKAir', paras = list(), func = 'UKAir'), 
                  list(module = 'UKAir', paras = list(a = 2, b = 'a'), func = 'UKAir'))
  
  expect_equal(e, eexpect)
  
  expect_true(exists('UKAir'))
})