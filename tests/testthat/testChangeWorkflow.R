context("Changing workflows")

test_that("ChangeWorkflow errors", {
  skip_on_cran()

  # Original run
  set.seed(1)
  w1 <- workflow(
    UKAnophelesPlumbeus,
    UKAir,
    Background(n = 70),
    LogisticRegression,
    PrintMap
  )

  expect_error(
    ChangeWorkflow(w1),
    "At least one module type must be changed"
  )

  expect_error(
    ChangeWorkflow(
      w1,
      occurrence = list(UKAnophelesPlumbeus, UKAnophelesPlumbeus),
      covariate = list(UKAir, UKAir)
    ),
    "Only one module type can be a list of multiple modules"
  )
})

test_that("Basic ChangeWorkflow works", {
  skip_on_cran()

  # Original run
  set.seed(1)
  w1 <- workflow(
    UKAnophelesPlumbeus,
    UKAir,
    Background(n = 70),
    LogisticRegression,
    PrintMap
  )

  # Change model
  set.seed(1)
  w2 <- workflow(
    UKAnophelesPlumbeus,
    UKAir,
    Background(n = 70),
    RandomForest,
    PrintMap
  )

  set.seed(1)
  w3 <- ChangeWorkflow(w2, model = LogisticRegression)

  expect_true(all.equal(
    w1[!names(w1) %in% "session.info"],
    w3[!names(w3) %in% "session.info"]
  ))
  expect_true(identical(
    w2$call,
    paste("workflow(occurrence = UKAnophelesPlumbeus, covariate = UKAir,",
          "process = Background(n = 70), model = RandomForest,",
          "output = PrintMap, forceReproducible = FALSE)")
  ))


  # Change Process, model and output
  set.seed(1)
  w4 <- workflow(
    UKAnophelesPlumbeus,
    UKAir,
    BackgroundAndCrossvalid(k = 2),
    RandomForest,
    PerformanceMeasures
  )

  set.seed(1)
  w5 <- ChangeWorkflow(
    w4,
    process = Background(n = 70),
    model = LogisticRegression,
    output = PrintMap
  )
  expect_true(all.equal(
    w1[!names(w1) %in% "session.info"],
    w5[!names(w5) %in% "session.info"]
  ))


  # Change occurrence and covariate
  set.seed(1)
  sink(file = ifelse(Sys.info()["sysname"] == "Windows",
    "NUL",
    "/dev/null"
  ))
  w6 <- workflow(
    NaiveRandomPresence,
    NaiveRandomRaster,
    Background(n = 70),
    LogisticRegression,
    PrintMap,
    forceReproducible = TRUE
  )
  sink()

  set.seed(1)
  sink(file = ifelse(Sys.info()["sysname"] == "Windows",
    "NUL",
    "/dev/null"
  ))
  w7 <- ChangeWorkflow(
    w6,
    occurrence = UKAnophelesPlumbeus,
    covariate = UKAir,
    forceReproducible = FALSE
  )
  sink()

  expect_true(all.equal(
    w1[!names(w1) %in% "session.info"],
    w7[!names(w7) %in% "session.info"]
  ))

  # Only change output
  # Change Process, model and output
  set.seed(1)
  w7a <- workflow(
    UKAnophelesPlumbeus,
    UKAir,
    Background(n = 70),
    LogisticRegression,
    PerformanceMeasures
  )
  set.seed(1)
  w7b <- ChangeWorkflow(
    w7a,
    output = PrintMap
  )
  expect_true(all.equal(
    w1[!names(w1) %in% "session.info"],
    w7b[!names(w7b) %in% "session.info"]
  ))
})

test_that("ChangeWorkflow - Chains", {
  skip_on_cran()

  # Original run
  set.seed(1)
  w1 <- workflow(
    UKAnophelesPlumbeus,
    UKAir,
    Background(n = 70),
    LogisticRegression,
    PrintMap
  )

  # Replace with a occurrence with chain
  set.seed(1)
  w8 <- workflow(
    Chain(UKAnophelesPlumbeus, UKAnophelesPlumbeus),
    UKAir,
    Background(n = 70),
    LogisticRegression,
    PrintMap
  )

  set.seed(1)
  w9 <- ChangeWorkflow(
    w1,
    occurrence = Chain(UKAnophelesPlumbeus, UKAnophelesPlumbeus)
  )

  expect_true(all.equal(
    w9[!names(w9) %in% "session.info"],
    w8[!names(w8) %in% "session.info"]
  ))

  # Replace with a covariate chain
  set.seed(1)
  w10 <- workflow(
    UKAnophelesPlumbeus,
    Chain(UKAir, UKAir),
    Background(n = 70),
    LogisticRegression,
    PrintMap
  )

  set.seed(1)
  w11 <- ChangeWorkflow(
    w1,
    covariate = Chain(UKAir, UKAir)
  )

  expect_true(
    all.equal(
      w10[!names(w10) %in% "session.info"],
      w11[!names(w11) %in% "session.info"]
    ),
    info = paste(
      all.equal(
        w10[!names(w10) %in% "session.info"],
        w11[!names(w11) %in% "session.info"]
      ),
      collapse = " *** "
    )
  )

  # Replace a chain with a non-chain
  set.seed(1)
  w12 <- ChangeWorkflow(
    w8,
    occurrence = UKAnophelesPlumbeus
  )

  expect_true(all.equal(
    w12[!names(w12) %in% "session.info"],
    w1[!names(w1) %in% "session.info"]
  ))
})

test_that("ChangeWorkflow - Lists", {
  skip_on_cran()

  # Original run
  set.seed(1)
  w1 <- workflow(
    UKAnophelesPlumbeus,
    UKAir,
    Background(n = 70),
    LogisticRegression,
    PrintMap
  )

  # Replace with a occurrence with chain
  set.seed(1)
  w13 <- workflow(
    list(UKAnophelesPlumbeus, UKAnophelesPlumbeus),
    UKAir,
    Background(n = 70),
    LogisticRegression,
    PrintMap
  )

  set.seed(1)
  w14 <- ChangeWorkflow(
    w1,
    occurrence = list(UKAnophelesPlumbeus, UKAnophelesPlumbeus)
  )

  expect_true(all.equal(
    w13[!names(w13) %in% "session.info"],
    w14[!names(w14) %in% "session.info"]
  ))

  # Replace with a covariate chain
  set.seed(1)
  w15 <- workflow(
    UKAnophelesPlumbeus,
    list(UKAir, UKAir),
    Background(n = 70),
    LogisticRegression,
    PrintMap
  )

  set.seed(1)
  w16 <- ChangeWorkflow(
    w1,
    covariate = list(UKAir, UKAir)
  )

  expect_true(all.equal(
    w15[!names(w15) %in% "session.info"],
    w16[!names(w16) %in% "session.info"]
  ))

  # Replace a chain with a non-chain
  set.seed(1)
  w17 <- ChangeWorkflow(
    w13,
    occurrence = UKAnophelesPlumbeus
  )

  expect_true(all.equal(
    w17[!names(w17) %in% "session.info"],
    w1[!names(w1) %in% "session.info"]
  ))
})

test_that("More complex syntax in remaining modules works", {
  skip_on_cran()

  # Original run
  set.seed(1)
  w1 <- workflow(
    UKAnophelesPlumbeus,
    UKAir,
    Background(n = 70),
    LogisticRegression,
    PrintMap
  )

  # Test module with brackets + args
  set.seed(1)
  w18 <- workflow(
    UKAnophelesPlumbeus,
    UKAir,
    BackgroundAndCrossvalid(k = 2),
    LogisticRegression,
    PrintMap
  )

  set.seed(1)
  w19 <- ChangeWorkflow(w18, occurrence = UKAnophelesPlumbeus)

  expect_true(all.equal(
    w18[!names(w18) %in% "session.info"],
    w19[!names(w19) %in% "session.info"]
  ))

  set.seed(1)
  w20 <- ChangeWorkflow(w1, process = BackgroundAndCrossvalid(k = 2))

  expect_true(all.equal(
    w20[!names(w20) %in% "session.info"],
    w18[!names(w18) %in% "session.info"]
  ))
})
