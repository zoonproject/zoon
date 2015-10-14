context('print.zoonWorkflow')

work1 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKAir,
                  process = OneHundredBackground,
                  model = LogisticRegression,
                  output = SameTimePlaceMap)

test_that('print.zoonWorkflow tests', {
  
  sum_out <- capture.output(print(work1))
  
  expect_identical(sum_out,
                   c("zoonWorkflow Object", "===================",
                     "",
                     "Call: workflow(occurrence = UKAnophelesPlumbeus, covariate = UKAir, process = OneHundredBackground, model = LogisticRegression, output = SameTimePlaceMap, forceReproducible = FALSE) "
                   ))
})

