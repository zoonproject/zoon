context("Transfrom CRS Function")

test_that("Expected errors", {
  basepath <- "https://raw.githubusercontent.com/zoonproject/modules/master/R/"
  NaiveRandomPresence <- source(paste0(basepath, "NaiveRandomPresence.R"))$value
  NaiveRandomRaster <- source(paste0(basepath, "NaiveRandomRaster.R"))$value

  occ_data <- NaiveRandomPresence(extent = c(0, 1000000, 0, 1000000),
                                  seed = 123)
  occ_data$crs <- "+init=epsg:27700"

  new_ras <- NaiveRandomRaster()
  new_ras <- projectRaster(new_ras, crs = CRS("+init=epsg:27700"))

  expect_error(
    zoon:::TransformCRS("tom", "+init=epsg:27700"),
    "occurrence must be a data.frame"
  )
  expect_error(
    zoon:::TransformCRS(occ_data, 27700),
    "ras_projection must be a character"
  )
  occ_data_error <- occ_data
  occ_data_error$crs <- "tom"
  expect_error(
    zoon:::TransformCRS(occ_data_error, "+init=epsg:27700"),
    "CRS provided in occurrence data"
  )
  expect_error(
    zoon:::TransformCRS(occ_data, "tom"),
    "CRS provided in covariate data"
  )
  expect_error(
    zoon:::TransformCRS(occ_data[, -6], "+init=epsg:27700"),
    'Transform CRS expects occurrence data to have a "crs" column'
  )

  # CRS column not given and they are not the same
  expect_error(workflow(
    occurrence = NaiveRandomPresence(
      extent = c(0, 1000000, 0, 1000000),
      seed = 123
    ),
    covariate = UKAir,
    process = Background(n = 70),
    model = LogisticRegression,
    output = PrintMap
  ))
})

test_that("occurrence data is handled as expected when CRSs vary", {
  latlong <- "+init=epsg:4326"
  eastnorth <- "+init=epsg:27700"

  occ_data <- NaiveRandomPresence(extent = c(0, 1000000, 0, 1000000),
                                  seed = 123)
  occ_data$crs <- "+init=epsg:27700"

  new_ras <- NaiveRandomRaster()
  new_ras <- projectRaster(new_ras, crs = CRS("+init=epsg:27700"))

  occ_data_new <- zoon:::TransformCRS(occ_data, "+init=epsg:4326")

  expect_is(occ_data_new, "data.frame")
  expect_equal(nrow(occ_data), nrow(occ_data_new),
               info = paste("Occurrence data has different number of rows",
                            "after coordinate transformation"))
  
  expect_identical(names(occ_data), names(occ_data_new),
                   paste("Occurrence data has different column names after",
                         "coordinate transformation"))

  t1 <- occ_data_new$longitude <= 180 & occ_data_new$longitude >= -180
  t2 <- occ_data_new$latitude <= 90 & occ_data_new$latitude >= -90

  expect_true(all(c(t1, t2)),
              info = "Some transformed values are not lat/long")

  expect_is(
    workflow(
      occurrence = NaiveRandomPresence(
        extent = c(-5, 5, 50, 55),
        seed = 123
      ),
      covariate = UKAir,
      process = Background(n = 70),
      model = LogisticRegression,
      output = PrintMap
    ),
    class = "zoonWorkflow",
    info = "lat/long raster and occurrence data with no CRS"
  )

  expect_is(
    workflow(
      occurrence = NaiveRandomPresence(
        extent = c(0, 1000000, 0, 1000000),
        seed = 123,
        projection = "+init=epsg:27700"
      ),
      covariate = UKAir,
      process = Background(n = 70),
      model = LogisticRegression,
      output = PrintMap
    ),
    class = "zoonWorkflow",
    info = "lat/long raster and e/n occurrence data"
  )

  expect_is(
    workflow(
      occurrence = list(
        NaiveRandomPresence(
          extent = c(0, 1000000, 0, 1000000),
          seed = 123,
          projection = "+init=epsg:27700"
        ),
        NaiveRandomPresence(extent = c(-5, 5, 50, 55), seed = 123)
      ),
      covariate = UKAir,
      process = Background(n = 70),
      model = LogisticRegression,
      output = PrintMap
    ),
    class = "zoonWorkflow",
    info = "lat/long raster and list of e/n occurrence data and with no CRS"
  )

  expect_is(
    workflow(
      occurrence = NaiveRandomPresence(extent = c(-5, 5, 50, 55), seed = 123),
      covariate = list(UKAir, NaiveRandomRaster),
      process = Background(n = 70),
      model = LogisticRegression,
      output = PrintMap
    ),
    class = "zoonWorkflow",
    info = "list of lat/long raster and occurrence data and with no CRS"
  )

  expect_is(
    workflow(
      occurrence = NaiveRandomPresence(
        extent = c(0, 1000000, 0, 1000000),
        seed = 123,
        projection = "+init=epsg:27700"
      ),
      covariate = list(UKAir, NaiveRandomRaster),
      process = Background(n = 70),
      model = LogisticRegression,
      output = PrintMap
    ),
    class = "zoonWorkflow",
    info = "list of lat/long raster and n/e occurrence data"
  )

  expect_is(
    workflow(
      occurrence = NaiveRandomPresence(
        extent = c(0, 1000000, 0, 1000000),
        seed = 123,
        projection = "+init=epsg:27700"
      ),
      covariate = list(
        NaiveRandomRaster,
        NaiveRandomRaster(
          extent = c(0, 1000000, 0, 1000000),
          seed = 123,
          res = 10000,
          projection = "+init=epsg:27700"
        )
      ),
      process = Background(n = 70),
      model = LogisticRegression,
      output = PrintMap
    ),
    class = "zoonWorkflow",
    info = "list of lat/long raster and n/e raster with n/e occurrence data"
  )
})

test_that("Handles NA values and blanks", {
  occ_data <- NaiveRandomPresence(
    extent = c(0, 1000000, 0, 1000000),
    seed = 123, projection = "+init=epsg:27700"
  )

  occ_data$latitude[1] <- NA
  occ_data$longitude[3] <- ""

  occ_data_new <- zoon:::TransformCRS(occ_data, "+init=epsg:4326")

  expect_equal(as.numeric(attr(na.omit(occ_data_new$longitude), "na.action")),
               c(1, 3))
})
