context("CombineRasters")

test_that("Test errors are thrown", {
  path <- paste0("https://raw.githubusercontent.com/zoonproject/modules",
                 "/master/R/NaiveRandomRaster.R")
  NaiveRandomRaster <- source(path)$value

  # Create some rasters for testing
  ras1 <- NaiveRandomRaster(
    extent = c(-10, 10, 45, 65),
    res = 1,
    seed = 123
  )

  ras2a <- NaiveRandomRaster(
    extent = c(-15, 15, 50, 70),
    res = 5,
    seed = 123
  )
  ras2b <- NaiveRandomRaster(
    extent = c(-15, 15, 50, 70),
    res = 5,
    seed = 124
  )
  ras2 <- stack(ras2a, ras2b)

  ras3 <- NaiveRandomRaster(
    extent = c(200000, 500000, 200000, 500000),
    res = 10000,
    seed = 123
  )
  projection(ras3) <- NA

  ras5 <- NaiveRandomRaster(
    extent = c(-40, -30, -20, 10),
    res = 10,
    seed = 123
  )

  expect_error(
    CombineRasters(list(ras1, ras2, ras3)),
    "1 covariate rasters do"
  )

  expect_error(
    CombineRasters(list(ras1, ras2, ras5)),
    "Rasters in covariates modules do not overlap"
  )
})

test_that("Combinations of projections and extents", {
  path <- paste0("https://raw.githubusercontent.com/zoonproject/modules",
                 "/master/R/NaiveRandomRaster.R")
  NaiveRandomRaster <- source(path)$value
  
  # Create some rasters for testing
  ras1 <- NaiveRandomRaster(
    extent = c(-10, 10, 45, 65),
    res = 1,
    seed = 123
  )

  ras2a <- NaiveRandomRaster(
    extent = c(-15, 15, 50, 70),
    res = 5,
    seed = 123
  )
  ras2b <- NaiveRandomRaster(
    extent = c(-15, 15, 50, 70),
    res = 5,
    seed = 124
  )
  ras2 <- stack(ras2a, ras2b)

  ras4 <- NaiveRandomRaster(
    extent = c(200000, 800000, 200000, 800000),
    res = 20000,
    seed = 123
  )
  projection(ras4) <- CRS("+init=epsg:27700")

  x <- CombineRasters(list(ras1, ras2, ras4))

  expect_is(x, "list")

  y <- stack(x)

  expect_is(y, "RasterStack")
})

test_that("Test in workflows", {

  # Create some rasters for testing
  work2 <- workflow(
    UKAnophelesPlumbeus,
    Chain(
      UKAir,
      NaiveRandomRaster
    ),
    OneHundredBackground,
    RandomForest,
    PrintMap
  )

  expect_is(work2, "zoonWorkflow")
})
