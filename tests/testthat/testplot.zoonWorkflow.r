context('plot.zoonwWorkflow')

dir <- tempdir()
dir.create(dir, showWarnings = FALSE)

test_that('plot.zoonWorkflow works', {

  # Create a simple workflow to test on
  work1 <- workflow(occurrence = UKAnophelesPlumbeus,
                    covariate = UKAir,
                    process = OneHundredBackground,
                    model = LogisticRegression,
                    output = SameTimePlaceMap)
  
  expect_null(zoon:::plot.zoonWorkflow(work1, dir = dir))
  ### use rplots.pdf to test plotting?
})

unlink(x = dir, recursive = TRUE)