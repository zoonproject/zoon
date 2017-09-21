context("Chain")

test_that("Chain tests", {
  ans <- Chain("test", "tom")

  expect_true(attr(ans, "chain"))
  expect_is(ans, "list")
})
