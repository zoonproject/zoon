context('summary.zoonWorkflow')

test_that('summary.zoonWorkflow tests', {
  skip_on_cran()

  set.seed(1)
  work1 <- workflow(occurrence = UKAnophelesPlumbeus,
                    covariate = UKAir,
                    process = Background(n=70),
                    model = LogisticRegression,
                    output = PrintMap)
    
  sum_ret <- summary(work1)
  
  expect_output(print(sum_ret), "Data summaries")

})