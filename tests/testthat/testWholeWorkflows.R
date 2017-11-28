context("Whole workflows")

expected_names <- c("occurrence.output",
                    "covariate.output",
                    "process.output",
                    "model.output",
                    "report",
                    "call",
                    "call.list",
                    "session.info",
                    "module.versions")

test_that("simple, package data workflow works.", {
  skip_on_cran()

  work1 <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = UKAir,
    process = Background(n = 70),
    model = LogisticRegression,
    output = PrintMap
  )

  expect_true(exists("work1"))
  expect_equal(names(work1), expected_names)

  expect_equal(dim(work1$occurrence.output[[1]]), c(188, 5))
  expect_is(work1$covariate.output[[1]], "RasterLayer")
  expect_equal(dim(work1$covariate.output[[1]]), c(9, 9, 1))
  column_names <- c("value", "type", "fold", "longitude", "latitude", "layer")
  expect_true(
    all(column_names %in% names(work1$process.output[[1]]$df))
  )
  expect_equal(dim(work1$process.output[[1]][[1]]), c(258, 6))
  expect_is((work1$model.output[[1]])$model, c("zoonModel"))
  expect_is((work1$model.output[[1]])$model$model, c("glm", "lm"))
  expect_is((work1$model.output[[1]])$data, c("data.frame"))
  expect_is(work1$report[[1]], "RasterLayer")
  expect_is(work1$session.info, "sessionInfo")
  expect_is(work1$module.versions, "list")
  expect_named(work1$module.versions,
               c("occurrence", "covariate", "process", "model", "output"))
})


test_that("Check basic quoted workflow.", {
  skip_on_cran()

  work1 <- workflow(
    occurrence = "UKAnophelesPlumbeus",
    covariate = "UKAir",
    process = "Background",
    model = "LogisticRegression",
    output = "PrintMap"
  )

  expect_true(exists("work1"))
  expect_equal(names(work1), expected_names)
  expect_equal(dim(work1$occurrence.output[[1]]), c(188, 5))
  expect_is(work1$covariate.output[[1]], "RasterLayer")
  expect_equal(dim(work1$covariate.output[[1]]), c(9, 9, 1))
  column_names <- c("value", "type", "fold", "longitude", "latitude", "layer")
  expect_true(
    all(column_names %in% names(work1$process.output[[1]]$df))
  )
  expect_equal(dim(work1$process.output[[1]][[1]]), c(269, 6))
  expect_is((work1$model.output[[1]])$model, c("zoonModel"))
  expect_is((work1$model.output[[1]])$model$model, c("glm", "lm"))
  expect_is((work1$model.output[[1]])$data, c("data.frame"))
  expect_is(work1$report[[1]], "RasterLayer")
  expect_is(work1$session.info, "sessionInfo")
  expect_is(work1$module.versions, "list")
  expect_named(work1$module.versions,
               c("occurrence", "covariate", "process", "model", "output"))
})

test_that("modules downloading data work", {
  skip_on_cran()

  work2 <- workflow(
    occurrence = Lorem_ipsum_UK,
    covariate = UKAir,
    process = Background(n = 70),
    model = RandomForest,
    output = PrintMap
  )

  expect_true(exists("work2"))
  expect_equal(names(work2), expected_names)
  expect_is(work2$occurrence.output[[1]], "data.frame")
  expect_equal(names(work2$occurrence.output[[1]]),
               c("longitude", "latitude", "value", "type", "fold"))
  expect_true(all(work2$occurrence.output[[1]][, "longitude"] < 20))
  expect_true(all(work2$occurrence.output[[1]][, "longitude"] > -20))
  expect_true(all(work2$occurrence.output[[1]][, "latitude"] < 65))
  expect_true(all(work2$occurrence.output[[1]][, "latitude"] > 45))
  expect_true(all(work2$occurrence.output[[1]][, "type"] == "presence"))
  expect_is(work2$covariate.output[[1]], "RasterLayer")
  expect_is((work2$model.output[[1]])$model, "zoonModel")
  expect_is((work2$model.output[[1]])$model$model, "randomForest")
  expect_is(work2$report[[1]], "RasterLayer")
  expect_is(work2$session.info, "sessionInfo")
  expect_is(work2$module.versions, "list")
  expect_named(work2$module.versions,
               c("occurrence", "covariate", "process", "model", "output"))
})

test_that("Workflows with lists of modules work.", {
  skip_on_cran()
  
  classes <- function(x) {
    vapply(x, class, FUN.VALUE = "")
  }

  # Would like to remove some of the slow online database modules from here.
  # In fact I don't think the would pass cran.
  workOccurList <- workflow(
    occurrence = list(
      UKAnophelesPlumbeus,
      UKAnophelesPlumbeus
    ),
    covariate = UKAir,
    process = Background(n = 70),
    model = LogisticRegression,
    output = PrintMap
  )

  workCovarList <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = list(UKAir, UKAir),
    process = Background(n = 70),
    model = LogisticRegression,
    output = PrintMap
  )

  # There's only 1 appropriate process module at the moment!
  workProcessList <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = UKAir,
    process = list(Background(n = 70), Background(n = 70)),
    model = LogisticRegression,
    output = PrintMap
  )

  workModelList <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = UKAir,
    process = Background(n = 70),
    model = list(LogisticRegression, RandomForest),
    output = PrintMap
  )

  workOutputList <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = UKAir,
    process = Background(n = 70),
    model = LogisticRegression,
    output = list(PrintMap, PrintMap)
  )

  # Note session info is not tested [-8] as it varies from system
  # to system - most notably Travis
  expect_equivalent(vapply(workOccurList, length, FUN.VALUE = 1)[-8],
                    c(2, 1, 2, 2, 2, 1, 5, 5))
  expect_equivalent(vapply(workCovarList, length, FUN.VALUE = 1)[-8],
                    c(1, 2, 2, 2, 2, 1, 5, 5))
  expect_equivalent(vapply(workProcessList, length, FUN.VALUE = 1)[-8],
                    c(1, 1, 2, 2, 2, 1, 5, 5))
  expect_equivalent(vapply(workModelList, length, FUN.VALUE = 1)[-8],
                    c(1, 1, 1, 2, 2, 1, 5, 5))
  expect_equivalent(vapply(workOutputList, length, FUN.VALUE = 1)[-8],
                    c(1, 1, 1, 1, 2, 1, 5, 5))

  
  sub <- function (x) {
    x[!names(x) %in% "session.info"]
  }
    
  occurClasses <- unlist(lapply(sub(workOccurList), classes))
  covarClasses <- unlist(lapply(sub(workCovarList), classes))
  processClasses <- unlist(lapply(sub(workProcessList), classes))
  modelClasses <- unlist(lapply(sub(workModelList), classes))
  outputClasses <- unlist(lapply(sub(workOutputList), classes))

  expect_equivalent(occurClasses, c(
    "data.frame", "data.frame", "RasterLayer", "list",
    "list", "list", "list", "RasterLayer", "RasterLayer", "character",
    "list", "list", "list", "list", "list",
    "matrix", "matrix", "matrix", "matrix", "matrix"
  ))
  expect_equivalent(covarClasses, c(
    "data.frame", "RasterLayer", "RasterLayer", "list",
    "list", "list", "list", "RasterLayer", "RasterLayer", "character",
    "list", "list", "list", "list", "list",
    "matrix", "matrix", "matrix", "matrix", "matrix"
  ))
  expect_equivalent(processClasses, c(
    "data.frame", "RasterLayer", "list",
    "list", "list", "list", "RasterLayer", "RasterLayer", "character",
    "list", "list", "list", "list", "list",
    "matrix", "matrix", "matrix", "matrix", "matrix"
  ))
  expect_equivalent(modelClasses, c(
    "data.frame", "RasterLayer", "list",
    "list", "list", "RasterLayer", "RasterLayer", "character",
    "list", "list", "list", "list", "list",
    "matrix", "matrix", "matrix", "matrix", "matrix"
  ))
  expect_equivalent(outputClasses, c(
    "data.frame", "RasterLayer", "list",
    "list", "RasterLayer", "RasterLayer", "character",
    "list", "list", "list", "list", "list",
    "matrix", "matrix", "matrix", "matrix", "matrix"
  ))
})

test_that("only one set of multiple lists allowed.", {
  skip_on_cran()

  fnc1 <- function() {
    x <- workflow(
      occurrence = list(
        UKAnophelesPlumbeus,
        UKAnophelesPlumbeus
      ),
      covariate = list(UKAir, UKAir),
      process = Background(n = 70),
      model = LogisticRegression,
      output = PrintMap
    )
  }

  fnc2 <- function() {
    x <- workflow(
      occurrence = UKAnophelesPlumbeus,
      covariate = list(UKAir, UKAir),
      process = list(Background(n = 70), Background(n = 70)),
      model = LogisticRegression,
      output = PrintMap
    )
  }

  fnc3 <- function() {
    x <- workflow(
      occurrence = UKAnophelesPlumbeus,
      covariate = UKAir,
      process = Background(n = 70),
      model = list(LogisticRegression, LogisticRegression),
      output = list(PrintMap, PrintMap)
    )
  }

  expect_error(fnc1())
  expect_error(fnc2())
  expect_error(fnc3())
})


test_that("simple, crossvalidation workflow works.", {
  skip_on_cran()

  workCross <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = UKAir,
    process = BackgroundAndCrossvalid,
    model = LogisticRegression,
    output = PrintMap
  )

  expect_true(exists("workCross"))
  expect_equal(names(workCross), expected_names)
  expect_equal(dim(workCross$occurrence.output[[1]]), c(188, 5))
  expect_is(workCross$covariate.output[[1]], "RasterLayer")
  expect_equal(dim(workCross$covariate.output[[1]]), c(9, 9, 1))
  expect_equal(
    names(workCross$process.output[[1]]$df),
    c("value", "type", "fold", "longitude", "latitude", "layer")
  )
  expect_equal(dim(workCross$process.output[[1]]$df), c(269, 6))
  expect_is((workCross$model.output[[1]])$model, c("zoonModel"))
  expect_is((workCross$model.output[[1]])$model$model, c("glm", "lm"))
  expect_is(workCross$report[[1]], "RasterLayer")
  expect_is(workCross$session.info, "sessionInfo")
  expect_is(workCross$module.versions, "list")
  expect_named(workCross$module.versions,
               c("occurrence", "covariate", "process", "model", "output"))
})

test_that("chains work.", {
  skip_on_cran()

  chain1 <- workflow(
    occurrence = Chain(UKAnophelesPlumbeus, UKAnophelesPlumbeus),
    covariate = UKAir,
    process = Background(n = 70),
    model = LogisticRegression,
    output = PrintMap
  )


  chain2 <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = Chain(UKAir, UKAir),
    process = Background(n = 70),
    model = LogisticRegression,
    output = PrintMap
  )

  chain3 <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = Chain(UKAir, UKAir),
    process = Background(n = 70),
    model = LogisticRegression,
    output = Chain(PrintMap, PrintMap)
  )

  chain4 <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = UKAir,
    process = Background(n = 70),
    model = LogisticRegression,
    output = Chain(PrintMap, PrintMap)
  )

  chain5 <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = UKAir,
    process = Chain(Background(n = 70), NoProcess),
    model = LogisticRegression,
    output = PrintMap
  )

  expect_true(exists("chain1"))
  expect_equal(dim(chain1$occurrence.output[[1]]), c(376, 5))
  expect_is(chain1$covariate.output[[1]], "RasterLayer")
  expect_equal(dim(chain1$covariate.output[[1]]), c(9, 9, 1))
  column_names <- c("value", "type", "fold", "longitude", "latitude", "layer")
  expect_true(
    all(column_names %in% names(chain1$process.output[[1]]$df))
  )
  expect_equal(dim(chain1$process.output[[1]]$df), c(446, 6))
  expect_is((chain1$model.output[[1]])$model, c("zoonModel"))
  expect_is((chain1$model.output[[1]])$model$model, c("glm", "lm"))
  expect_is(chain1$report[[1]], "RasterLayer")
  expect_is(chain1$session.info, "sessionInfo")
  expect_is(chain1$module.versions, "list")
  expect_named(chain1$module.versions,
               c("occurrence", "covariate", "process", "model", "output"))


  expect_true(exists("chain2"))
  expect_equal(dim(chain2$occurrence.output[[1]]), c(188, 5))
  expect_is(chain2$covariate.output[[1]], "RasterStack")
  expect_equal(dim(chain2$covariate.output[[1]]), c(9, 9, 2))
  column_names <- c("value", "type", "fold", "longitude", "latitude", "layer.1", "layer.2")
  expect_true(
    all(column_names %in% names(chain2$process.output[[1]]$df))
  )
  expect_equal(dim(chain2$process.output[[1]]$df), c(258, 7))
  expect_is((chain2$model.output[[1]])$model, c("zoonModel"))
  expect_is((chain2$model.output[[1]])$model$model, c("glm", "lm"))
  expect_is(chain2$report[[1]], "RasterLayer")
  expect_is(chain2$session.info, "sessionInfo")
  expect_is(chain2$module.versions, "list")
  expect_named(chain2$module.versions,
               c("occurrence", "covariate", "process", "model", "output"))


  expect_true(exists("chain3"))
  expect_equal(dim(chain3$occurrence.output[[1]]), c(188, 5))
  expect_is(chain3$covariate.output[[1]], "RasterStack")
  expect_equal(dim(chain3$covariate.output[[1]]), c(9, 9, 2))
  column_names <- c("value", "type", "fold", "longitude", "latitude", "layer.1", "layer.2")
  expect_true(
    all(column_names %in% names(chain3$process.output[[1]]$df))
  )
  expect_equal(dim(chain3$process.output[[1]]$df), c(258, 7))
  expect_is((chain3$model.output[[1]])$model, c("zoonModel"))
  expect_is((chain3$model.output[[1]])$model$model, c("glm", "lm"))
  expect_is(chain3$report[[1]], "list")
  expect_is(chain3$session.info, "sessionInfo")
  expect_is(chain3$module.versions, "list")
  expect_named(chain3$module.versions,
               c("occurrence", "covariate", "process", "model", "output"))


  expect_true(exists("chain4"))
  expect_equal(dim(chain4$occurrence.output[[1]]), c(188, 5))
  expect_is(chain4$covariate.output[[1]], "RasterLayer")
  expect_equal(dim(chain4$covariate.output[[1]]), c(9, 9, 1))
  column_names <- c("value", "type", "fold", "longitude", "latitude", "layer")
  expect_true(
    all(column_names %in% names(chain4$process.output[[1]]$df))
  )
  expect_equal(dim(chain4$process.output[[1]]$df), c(258, 6))
  expect_is((chain4$model.output[[1]])$model, c("zoonModel"))
  expect_is((chain4$model.output[[1]])$model$model, c("glm", "lm"))
  expect_is(chain4$report[[1]], "list")
  expect_is(chain4$session.info, "sessionInfo")
  expect_is(chain4$module.versions, "list")
  expect_named(chain4$module.versions,
               c("occurrence", "covariate", "process", "model", "output"))


  expect_true(exists("chain5"))
  expect_equal(dim(chain5$occurrence.output[[1]]), c(188, 5))
  expect_is(chain5$covariate.output[[1]], "RasterLayer")
  expect_equal(dim(chain5$covariate.output[[1]]), c(9, 9, 1))
  column_names <- c("value", "type", "fold", "longitude", "latitude", "layer")
  expect_true(
    all(column_names %in% names(chain5$process.output[[1]]$df))
  )
  expect_equal(dim(chain5$process.output[[1]]$df), c(258, 6))
  expect_is((chain5$model.output[[1]])$model, c("zoonModel"))
  expect_is((chain5$model.output[[1]])$model$model, c("glm", "lm"))
  expect_is(chain5$report[[1]], "RasterLayer")
  expect_is(chain5$session.info, "sessionInfo")
  expect_is(chain5$module.versions, "list")
  expect_named(chain5$module.versions,
               c("occurrence", "covariate", "process", "model", "output"))
})



test_that("workflow with mix of syntax works.", {
  skip_on_cran()

  workSyn <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = "UKAir",
    process = BackgroundAndCrossvalid(k = 2),
    model = list(LogisticRegression, RandomForest),
    output = Chain("PrintMap", "PrintMap")
  )

  expect_true(exists("workSyn"))
  expect_equal(names(workSyn), expected_names)
  expect_equal(dim(workSyn$occurrence.output[[1]]), c(188, 5))
  expect_is(workSyn$covariate.output[[1]], "RasterLayer")
  expect_equal(dim(workSyn$covariate.output[[1]]), c(9, 9, 1))
  expect_equal(
    names(workSyn$process.output[[1]]$df),
    c("value", "type", "fold", "longitude", "latitude", "layer")
  )
  expect_equal(dim(workSyn$process.output[[1]][[1]]), c(269, 6))
  expect_is((workSyn$model.output[[1]])$model, c("zoonModel"))
  expect_is((workSyn$model.output[[1]])$model$model, c("glm", "lm"))
  expect_is((workSyn$model.output[[1]])$data, c("data.frame"))
  expect_is(workSyn$report[[1]], "RasterLayer")
  expect_is(workSyn$session.info, "sessionInfo")
  expect_is(workSyn$module.versions, "list")
  expect_named(workSyn$module.versions,
               c("occurrence", "covariate", "process", "model", "output"))
})


test_that("Output understands which previous model was listed.", {
  skip_on_cran()
  
  classes <- function(x) {
    vapply(x, class, FUN.VALUE = "")
  }

  # See issue 263 for discussion
  # https://github.com/zoonproject/zoon/issues/263

  # Create a local raster *with a differently named layer*
  #   The listed covariate tests above erroneously passed because we
  #   listed UKAir twice, so they had identical layer names.
  UKAirRas2 <<- UKAirRas
  names(UKAirRas2) <- "NewName"

  work1 <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = list(LocalRaster(UKAirRas2), UKAir),
    process = Background(n = 70),
    model = LogisticRegression,
    output = PrintMap
  )

  work2 <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = list(LocalRaster(UKAirRas2), UKAir),
    process = Background(n = 70),
    model = LogisticRegression,
    output = Chain(PrintMap, PrintMap)
  )

  rm(list = c("UKAirRas2"))

  expect_equivalent(vapply(work1, length, FUN.VALUE = 1)[-8],
                    c(1, 2, 2, 2, 2, 1, 5, 5))
  
  sub <- function (x) {
    x[!names(x) %in% "session.info"]
  }
  
  
  covarClasses1 <- unlist(lapply(sub(work1), classes))

  expect_equivalent(covarClasses1, c(
    "data.frame", "RasterLayer", "RasterLayer", "list",
    "list", "list", "list", "RasterLayer", "RasterLayer", "character",
    "list", "list", "list", "list", "list",
    "matrix", "matrix", "matrix", "matrix", "matrix"
  ))



  expect_equivalent(vapply(work2, length, FUN.VALUE = 1)[-8],
                    c(1, 2, 2, 2, 2, 1, 5, 5))

  covarClasses2 <- unlist(lapply(sub(work2), classes))

  expect_equivalent(covarClasses2, c(
    "data.frame", "RasterLayer", "RasterLayer", "list",
    "list", "list", "list", "list", "list", "character",
    "list", "list", "list", "list", "list",
    "matrix", "matrix", "matrix", "matrix", "matrix"
  ))
})


test_that("Running modules with parameters", {
  skip_on_cran()

  # I dont think we do this elsewhere
})
