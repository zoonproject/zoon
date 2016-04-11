context('print.zoonSummary')

test_that('print.zoonSummary tests', {

  set.seed(1)
  work1 <- workflow(occurrence = UKAnophelesPlumbeus,
                    covariate = UKAir,
                    process = OneHundredBackground,
                    model = LogisticRegression,
                    output = PrintMap)
  
  sum_out <- capture.output(summary(work1))
  
  expect_output(print(sum_out), "Data summaries")
  
})

