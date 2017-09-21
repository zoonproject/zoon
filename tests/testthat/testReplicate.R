context("Replicate")

test_that("Replicate tests", {
  t3 <- Replicate(tom(), 3)
  expect_identical(t3,
                   list(substitute(tom()),
                        substitute(tom()),
                        substitute(tom())))

  t3_a <- Replicate(tom(k = 1, t = "same"), 3)
  expect_identical(t3, list(
    substitute(tom()),
    substitute(tom()),
    substitute(tom())
  ))
  expect_identical(t3_a, list(
    substitute(tom(k = 1, t = "same")),
    substitute(tom(k = 1, t = "same")),
    substitute(tom(k = 1, t = "same"))
  ))

  expect_error(Replicate("tom"),
               'argument "n" is missing, with no default')
})

test_that("Replicate workflow tests", {
  work2 <- workflow(
    occurrence = Replicate(UKAnophelesPlumbeus, 3),
    covariate = UKAir,
    process = Background(n = 70),
    model = LogisticRegression,
    output = SameTimePlaceMap
  )

  expect_equal(length(Occurrence(work2)), 3)
  expect_equal(length(Model(work2)), 3)

  work1 <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = UKAir,
    process = Replicate(Background(n = 10), n = 10),
    model = LogisticRegression,
    output = PrintMap
  )

  expect_equal(length(Model(work1)), 10)
})
