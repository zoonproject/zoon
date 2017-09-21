context("SortArgs")


test_that("SortArgs works.", {
  x <- SortArgs(
    PasteAndDep(substitute(UKAnophelesPlumbeus)),
    PasteAndDep(substitute(UKAir)),
    PasteAndDep(substitute(OneHundredBackground)),
    PasteAndDep(substitute(LogisticRegression)),
    PasteAndDep(substitute(PrintMap)),
    TRUE
  )

  expect_equal(x,
               paste("workflow(occurrence = UKAnophelesPlumbeus,",
                     "covariate = UKAir, process = OneHundredBackground,",
                     "model = LogisticRegression, output = PrintMap,",
                     "forceReproducible = TRUE)"))

  # Check the call is runeable
  w <- eval(parse(text = x))
  expect_true(inherits(w, "zoonWorkflow"))
  expect_false(any(vapply(w, is.null, FUN.VALUE = FALSE)))


  # More complex syntax
  y <- SortArgs(
    PasteAndDep(substitute(UKAnophelesPlumbeus)),
    PasteAndDep(substitute("UKAir")),
    PasteAndDep(substitute(BackgroundAndCrossvalid(k = 2))),
    PasteAndDep(substitute(list(LogisticRegression, LogisticRegression))),
    PasteAndDep(substitute(Chain(PrintMap, PrintMap))),
    TRUE
  )

  expect_true(length(y) == 1)
  expect_true(inherits(y, "character"))

  # Check the call is runeable
  w2 <- eval(parse(text = y))
  expect_true(inherits(w2, "zoonWorkflow"))
  expect_false(any(vapply(w2, is.null, FUN.VALUE = FALSE)))
})
