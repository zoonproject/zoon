test_that('summary.zoonWorkflow tests', {

  context('summary.zoonWorkflow')
  
  set.seed(1)
  work1 <- workflow(occurrence = UKAnophelesPlumbeus,
                    covariate = UKAir,
                    process = OneHundredBackground,
                    model = LogisticRegression,
                    output = PrintMap)
    
  sum_ret <- summary(work1)
  
  expect_output(summary(work1), "Data summaries")

})