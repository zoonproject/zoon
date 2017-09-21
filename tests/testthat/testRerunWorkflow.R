context("RerunWorkflow")

# NOTE: forceReproducible is used here to ensure that modules
# are not cached as this results in a change in the version number
# from x to 'local copy'


test_that("RerunWorkflow simple test", {
  skip_on_cran()

  set.seed(1)
  w1 <- workflow(
    UKAnophelesPlumbeus,
    UKAir,
    Background(n = 70),
    LogisticRegression,
    PrintMap,
    forceReproducible = TRUE
  )

  set.seed(1)
  w2 <- RerunWorkflow(w1)

  expect_true(all.equal(w1, w2))
})


test_that("RerunWorkflow test error", {
  skip_on_cran()
  set.seed(1)
  w1 <- workflow(
    UKAnophelesPlumbeus,
    UKAir,
    Background(n = 70),
    LogisticRegression,
    PrintMap,
    forceReproducible = TRUE
  )

  set.seed(1)

  expect_error(w2 <- RerunWorkflow(w1, from = "a"),
               regexp = "from \\%in\\% c\\(1\\:5\\) is not TRUE")
  expect_error(w2 <- RerunWorkflow(w1, from = 6),
               regexp = "from \\%in\\% c\\(1\\:5\\) is not TRUE")

  # You cannot re-run a workflow with
})


test_that("RerunWorkflow test with NULLs", {
  skip_on_cran()

  set.seed(1)
  w1 <- workflow(
    UKAnophelesPlumbeus,
    UKAir,
    Background(n = 70),
    LogisticRegression,
    PrintMap,
    forceReproducible = TRUE
  )

  w3 <- w1
  for (i in 1:5) w3[i] <- list(NULL)

  w4 <- w1
  for (i in 2:5) w4[i] <- list(NULL)

  w5 <- w1
  for (i in 3:5) w5[i] <- list(NULL)

  w6 <- w1
  for (i in 4:5) w6[i] <- list(NULL)

  w7 <- w1
  for (i in 5) w7[i] <- list(NULL)

  set.seed(1)
  w8 <- RerunWorkflow(w3)
  set.seed(1)
  w9 <- RerunWorkflow(w4)
  set.seed(1)
  w10 <- RerunWorkflow(w5)
  set.seed(1)
  w11 <- RerunWorkflow(w6)
  set.seed(1)
  w12 <- RerunWorkflow(w7)

  expect_true(all.equal(w1, w8))
  expect_true(all.equal(w1, w9))
  expect_true(all.equal(w1, w10))
  expect_true(all.equal(w1, w11))
  expect_true(all.equal(w1, w12))
})


test_that("RerunWorkflow test with Chains", {
  skip_on_cran()

  set.seed(1)
  w13 <- workflow(
    Chain(UKAnophelesPlumbeus, UKAnophelesPlumbeus),
    UKAir,
    Background(n = 70),
    LogisticRegression,
    PrintMap,
    forceReproducible = TRUE
  )

  set.seed(1)
  w14 <- RerunWorkflow(w13)
  expect_true(all.equal(w14, w13))

  set.seed(1)
  w15 <- workflow(
    Chain(UKAnophelesPlumbeus, UKAnophelesPlumbeus),
    Chain(UKAir, UKAir),
    Background(n = 70),
    LogisticRegression,
    PrintMap,
    forceReproducible = TRUE
  )

  set.seed(1)
  w16 <- RerunWorkflow(w15)
  expect_true(all.equal(w15, w16))
})

test_that("RerunWorkflow test with lists", {
  skip_on_cran()

  set.seed(1)
  w17 <- workflow(
    list(UKAnophelesPlumbeus, UKAnophelesPlumbeus),
    UKAir,
    Background(n = 70),
    LogisticRegression,
    PrintMap,
    forceReproducible = TRUE
  )

  set.seed(1)
  w18 <- RerunWorkflow(w17)
  expect_true(all.equal(w18, w17))

  set.seed(1)
  w19 <- workflow(
    UKAnophelesPlumbeus,
    list(UKAir, UKAir),
    Background(n = 70),
    LogisticRegression,
    PrintMap,
    forceReproducible = TRUE
  )

  set.seed(1)
  w20 <- RerunWorkflow(w19)
  expect_true(all.equal(w19, w20))
})

test_that("RerunWorkflow test quoted modules", {
  skip_on_cran()

  set.seed(1)
  w21 <- workflow(
    occurrence = "UKAnophelesPlumbeus",
    covariate = UKAir,
    process = Background(n = 70),
    model = RandomForest,
    output = PrintMap,
    forceReproducible = TRUE
  )

  set.seed(1)
  w22 <- RerunWorkflow(w21)

  expect_true(all.equal(w21, w22))
})
