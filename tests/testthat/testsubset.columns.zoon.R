context('subset.columns.zoon')

test_that('subset.columns.zoon testing', {
  
  # create data.frames
  a <- data.frame(1:10, letters = letters[1:10], 10:1)
  attr(a, 'test') <- TRUE
  attr(a, 'test1') <- 1:3
  
  # we expect subsetting to drop the attributes
  expect_true(is.null(attr(a[ ,c(1,2)], 'test')))
  
  # we expect subset.columns.zoon to keep the attributes
  expect_equal(attr(subset.columns.zoon(a, columns = c(1,3)), 'test'), TRUE)
  expect_equal(attr(subset.columns.zoon(a, columns = 'letters'), 'test1'), 1:3)
  
})
