context("SplitCall")

test_that("SplitCall works", {
  # This function is only ever used on calls from a workflow object, so we can
  # use SortArgs to simulate the possible things we might get.


  call1 <- SortArgs(
    PasteAndDep(substitute(UKAnophelesPlumbeus)),
    PasteAndDep(substitute(UKAir)),
    PasteAndDep(substitute(OneHundredBackground)),
    PasteAndDep(substitute(LogisticRegression)),
    PasteAndDep(substitute(PrintMap)),
    TRUE
  )


  split1 <- SplitCall(call1)

  expect_true(inherits(split1, "character"))
  expect_equal(length(split1), 6)


  # Test argument inputs, lists, chains and character inputs.
  call2 <- SortArgs(
    PasteAndDep(substitute(UKAnophelesPlumbeus)),
    PasteAndDep(substitute("UKAir")),
    PasteAndDep(substitute(BackgroundAndCrossvalid(k = 2))),
    PasteAndDep(substitute(list(LogisticRegression,
                                LogisticRegression))),
    PasteAndDep(substitute(Chain(PrintMap, PrintMap))),
    TRUE
  )

  split2 <- SplitCall(call2)

  expect_true(inherits(split2, "character"))
  expect_equal(length(split2), 6)



  # Test List of arguments, list of characters
  call3 <- SortArgs(
    PasteAndDep(substitute(UKAnophelesPlumbeus(k = "awd", v = 2))),
    PasteAndDep(substitute(Chain("UKAir", "UKAir"))),
    PasteAndDep(substitute(list(BackgroundAndCrossvalid(k = 2),
                                BackgroundAndCrossvalid(k = 2, l = 3)))),
    PasteAndDep(substitute(list(LogisticRegression,
                                LogisticRegression))),
    PasteAndDep(substitute(Chain(PrintMap(l = 2),
                                 PrintMap(l = 2, k = 3, r = "23")))),
    TRUE
  )

  split3 <- SplitCall(call3)

  expect_true(inherits(split3, "character"))
  expect_equal(length(split3), 6)
})
