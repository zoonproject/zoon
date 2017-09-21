context("ErrorModule")

test_that("ErrorModule works.", {

  # As this throws errors it's quite hard to test. To properly test that
  # tmpZoonWorkflow is returned properly is a pain as well Probably easier to
  # test this thoroughly in test-wholeWorkflow by breaking workflows.

  expect_error(ErrorModule("esv", 1, environment()),
               "Stopping workflow due to error in")

  expect_false(exists("tmpZoonWorkflow"))
})
