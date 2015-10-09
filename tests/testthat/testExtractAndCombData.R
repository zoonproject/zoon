context('ExtractAndCombData')

test_that('ExtractAndCombData works.', {
  
  x <- ExtractAndCombData(head(AplumbeusOcc), UKAirRas)
  
  expect_true(inherits(x, 'list'))
  expect_true(all.equal(names(x), c('df', 'ras')))
  expect_true(all.equal(dim(x$df), c(6,6)))
  
  # Give an error if trying to use occurrence data outside raster extent
  outOfExtentData <- head(AplumbeusOcc)
  outOfExtentData$longitude <- -20
  
  expect_error(ExtractAndCombData(outOfExtentData, UKAirRas))
  
})
