context("ZoonCitation")

test_that("Check error if not found", {
  skip_on_cran()

  expect_error(
    ZoonCitation("ThisDoesNotExist"),
    "URL for module does not exist"
  )
})

test_that("Check returns list with all the names", {
  x <- ZoonCitation("LogisticRegression")

  names <- c(
    "title", "name", "authors",
    "date_submitted", "note", "email",
    "url"
  )

  expect_named(x, names)
})
