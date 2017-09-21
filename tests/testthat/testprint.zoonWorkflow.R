context("print.zoonWorkflow")

test_that("print.zoonWorkflow tests", {
  skip_on_cran()

  set.seed(1)
  work1 <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = UKAir,
    process = Background(n = 70),
    model = LogisticRegression,
    output = PrintMap
  )

  sum_out <- capture.output(print(work1))

  expect_identical(
    sum_out,
    c("zoonWorkflow Object", "===================",
      "",
      paste("Call: workflow(occurrence = UKAnophelesPlumbeus,",
            "covariate = UKAir, process = Background(n = 70),",
            "model = LogisticRegression, output = PrintMap,",
            "forceReproducible = FALSE) ")
    )
  )
})
