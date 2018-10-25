context("cbindZoon")

test_that("cbindZoon testing", {

  # create data.frames
  a <- data.frame(1:10, letters[1:10])
  attr(a, "test") <- TRUE
  attr(a, "test1") <- 1:3

  b <- data.frame(10:1, letters[10:1])

  # we expect cbind to drop the attributes
  expect_true(is.null(attr(cbind(a, b), "test")))

  # we expect cbindZoon to keep the attributes
  expect_equal(attr(cbindZoon(a, b), "test"), TRUE)
  expect_equal(attr(cbindZoon(a, b), "test1"), 1:3)
})
