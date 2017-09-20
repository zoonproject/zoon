context("LapplyGetModule")

test_that("LapplyGetModule works correctly", {
  a <- list(module = "UKAir", paras = list())
  b <- list(module = "UKAir", paras = list())

  c <- LapplyGetModule(list(a, b), forceReproducible = FALSE)

  expect_is(c, "list")
  expect_equal(names(c[[1]]), c("module", "paras", "func", "version"))
  expect_equal(names(c[[2]]), c("module", "paras", "func", "version"))
  expect_equal(c[[1]]$module, "UKAir")
  expect_equal(c[[2]]$module, "UKAir")
  expect_equal(c[[1]]$paras, list())
  expect_equal(c[[2]]$paras, list())
  expect_equal(c[[1]]$func, "UKAir")
  expect_equal(c[[2]]$func, "UKAir")

  d <- list(module = "UKAir", paras = list(a = 2, b = "a"))

  e <- LapplyGetModule(list(a, d), forceReproducible = FALSE)

  expect_is(e, "list")
  expect_equal(names(e[[2]]), c("module", "paras", "func", "version"))
  expect_equal(e[[2]]$module, "UKAir")
  expect_equal(e[[2]]$paras, list(a = 2, b = "a"))
  expect_equal(e[[2]]$func, "UKAir")

  expect_true(exists("UKAir"))
})
