context("GetModuleList")

test_that("GetModuleList renew", {
  if (!capabilities("libcurl")) skip("skipping as libcurl not supported")

  modList <- GetModuleList(renew = TRUE)

  expect_is(modList, "list")
  expect_identical(
    names(modList),
    c("occurrence", "covariate", "process", "model", "output")
  )

  expect_true("LocalOccurrenceData" %in% modList$occurrence)
  expect_true("LocalRaster" %in% modList$covariate)
  expect_true("NoProcess" %in% modList$process)
  expect_true("LogisticRegression" %in% modList$model)
  expect_true("PrintMap" %in% modList$output)
})

test_that("GetModuleList no renew", {
  if (!capabilities("libcurl")) skip("skipping as libcurl not supported")

  modList <- GetModuleList(renew = FALSE)

  expect_is(modList, "list")
  expect_identical(
    names(modList),
    c("occurrence", "covariate", "process", "model", "output")
  )

  expect_true("LocalOccurrenceData" %in% modList$occurrence)
  expect_true("LocalRaster" %in% modList$covariate)
  expect_true("NoProcess" %in% modList$process)
  expect_true("LogisticRegression" %in% modList$model)
  expect_true("PrintMap" %in% modList$output)
})

test_that("GetModuleList type occurrence", {
  if (!capabilities("libcurl")) skip("skipping as libcurl not supported")

  modList <- GetModuleList(type = "occurrence")

  expect_is(modList, "character")

  expect_true("LocalOccurrenceData" %in% modList)
})

test_that("GetModuleList type covariate", {
  if (!capabilities("libcurl")) skip("skipping as libcurl not supported")

  modList <- GetModuleList(type = "covariate")

  expect_is(modList, "character")

  expect_true("LocalRaster" %in% modList)
})

test_that("GetModuleList type process", {
  if (!capabilities("libcurl")) skip("skipping as libcurl not supported")

  modList <- GetModuleList(type = "process")

  expect_is(modList, "character")

  expect_true("NoProcess" %in% modList)
})

test_that("GetModuleList type model", {
  if (!capabilities("libcurl")) skip("skipping as libcurl not supported")

  modList <- GetModuleList(type = "model")

  expect_is(modList, "character")

  expect_true("LogisticRegression" %in% modList)
})

test_that("GetModuleList type ouput", {
  if (!capabilities("libcurl")) skip("skipping as libcurl not supported")

  modList <- GetModuleList(type = "output")

  expect_is(modList, "character")

  expect_true("PrintMap" %in% modList)
})
