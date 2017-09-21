
context("Accessor functions")

test_that(paste("All accessor functions return objects",
                "of the right length and type"), {

  # Three work flows that cover all lengths (1 or > 1) of outputs.

  # All length 1 (therefore not lists of length 1)
  work1 <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = UKAir,
    process = Background(n = 70),
    model = LogisticRegression,
    output = PrintMap
  )


  # All length 2 except covariate
  work2 <- workflow(
    occurrence = list(UKAnophelesPlumbeus, UKAnophelesPlumbeus),
    covariate = UKAir,
    process = Background(n = 70),
    model = LogisticRegression,
    output = PrintMap
  )


  # All length 2 expect occurrence
  work3 <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = list(UKAir, UKAir),
    process = Background(n = 70),
    model = LogisticRegression,
    output = PrintMap
  )


  # occurrence

  expect_false(inherits(Occurrence(work1), "list"))
  expect_true(inherits(Occurrence(work1), "data.frame"))

  expect_true(length(Occurrence(work2)) == 2)
  expect_true(inherits(Occurrence(work2), "list"))



  # covariate

  expect_false(inherits(Covariate(work1), "list"))
  expect_true(inherits(Covariate(work1), "RasterLayer"))

  expect_true(length(Covariate(work3)) == 2)
  expect_true(inherits(Covariate(work3), "list"))


  # process

  expect_true(inherits(Process(work1), "list"))
  expect_true(inherits(Process(work1)[[1]], "data.frame"))
  expect_true(inherits(Process(work1)[[2]], "RasterLayer"))

  expect_true(length(Process(work2)) == 2)
  expect_true(inherits(Process(work2), "list"))
  expect_true(inherits(Process(work2)[[1]], "list"))
  expect_true(inherits(Process(work2)[[2]], "list"))
  expect_true(inherits(Process(work2)[[1]][[1]], "data.frame"))
  expect_true(inherits(Process(work2)[[1]][[2]], "RasterLayer"))


  # model

  expect_true(inherits(Model(work1), "list"))
  expect_true(inherits(Model(work1)[[2]], "data.frame"))
  expect_true(inherits(Model(work1)[[1]], "zoonModel"))

  expect_true(length(Model(work2)) == 2)
  expect_true(inherits(Model(work2), "list"))
  expect_true(inherits(Model(work2)[[1]], "list"))
  expect_true(inherits(Model(work2)[[2]], "list"))
  expect_true(inherits(Model(work2)[[1]][[2]], "data.frame"))
  expect_true(inherits(Model(work2)[[1]][[1]], "zoonModel"))


  # output

  expect_false(inherits(Output(work1), "list"))
  expect_true(inherits(Output(work1), "RasterLayer"))

  expect_true(length(Output(work3)) == 2)
  expect_true(inherits(Output(work3), "list"))
  expect_true(inherits(Output(work3)[[1]], "RasterLayer"))
})
