context("print.zoonCitation")

test_that("print.zoonCitation tests", {
  skip_on_cran()

  cit_out <- capture.output(ZoonCitation("LogisticRegression"))

  expect_output(print(cit_out),
                "To cite the LogisticRegression module in publications use:")
})
