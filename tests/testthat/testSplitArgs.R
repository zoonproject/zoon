context("SplitArgs")

test_that("SplitArgs works", {

  a <- SplitArgs("moduleName(parameter = 2, parameter2 = 3)")
  b <- SplitArgs("moduleName")

  # Interesting to note that SplitArgs doesn't deal with numerics at all.
  #   Probably needs looking into.
  expect_equal(a,
               list(module = "moduleName",
                    paras = list(parameter = "2",
                                 parameter2 = "3")))
  expect_equal(b,
               list(module = "moduleName",
                    paras = list()))
})
