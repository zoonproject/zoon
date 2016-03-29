
context('Test the accessor functions work properly')

test_that('All accessor functions return objects of the right length and type', {

  # Three work flows that cover all lengths (1 or > 1) of outputs.

  # All length 1 (therefore not lists of length 1)
  work1 <- workflow(occurrence = UKAnophelesPlumbeus,
                    covariate  = UKAir,
                    process    = OneHundredBackground,
                    model      = LogisticRegression,
                    output     = PrintMap)


  # All length 2 except covariate
  work2 <- workflow(occurrence = list(UKAnophelesPlumbeus, UKAnophelesPlumbeus),
                    covariate  = UKAir,
                    process    = OneHundredBackground,
                    model      = LogisticRegression,
                    output     = PrintMap)


  # All length 2 expect occurrence
  work3 <- workflow(occurrence = UKAnophelesPlumbeus,
                    covariate  = list(UKAir, UKAir),
                    process    = OneHundredBackground,
                    model      = LogisticRegression,
                    output     = PrintMap)


  # occurrence
  
  expect_false(inherits(occurrence(work1), 'list'))
  expect_true(inherits(occurrence(work1), 'data.frame'))

  expect_true(length(occurrence(work2)) == 2)
  expect_true(inherits(occurrence(work2), 'list'))



  # covariate
  
  expect_false(inherits(covariate(work1), 'list'))
  expect_true(inherits(covariate(work1), 'RasterLayer'))

  expect_true(length(covariate(work3)) == 2)
  expect_true(inherits(covariate(work3), 'list'))


  # process
  
  expect_true(inherits(process(work1), 'list'))
  expect_true(inherits(process(work1)[[1]], 'data.frame'))
  expect_true(inherits(process(work1)[[2]], 'RasterLayer'))

  expect_true(length(process(work2)) == 2)
  expect_true(inherits(process(work2), 'list'))
  expect_true(inherits(process(work2)[[1]], 'list'))
  expect_true(inherits(process(work2)[[2]], 'list'))
  expect_true(inherits(process(work2)[[1]][[1]], 'data.frame'))
  expect_true(inherits(process(work2)[[1]][[2]], 'RasterLayer'))


  # model
 
  expect_true(inherits(model(work1), 'list'))
  expect_true(inherits(model(work1)[[2]], 'data.frame'))
  expect_true(inherits(model(work1)[[1]], 'zoonModel'))

  expect_true(length(model(work2)) == 2)
  expect_true(inherits(model(work2), 'list'))
  expect_true(inherits(model(work2)[[1]], 'list'))
  expect_true(inherits(model(work2)[[2]], 'list'))
  expect_true(inherits(model(work2)[[1]][[2]], 'data.frame'))
  expect_true(inherits(model(work2)[[1]][[1]], 'zoonModel'))


  # output
  
  expect_false(inherits(output(work1), 'list'))
  expect_true(inherits(output(work1), 'RasterLayer'))

  expect_true(length(output(work3)) == 2)
  expect_true(inherits(output(work3), 'list'))
  expect_true(inherits(output(work3)[[1]], 'RasterLayer'))


})
