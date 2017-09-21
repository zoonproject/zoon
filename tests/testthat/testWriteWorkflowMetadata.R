context("WriteWorkflowMetadata")

test_that("WriteWorkflowMetadata works.", {
  work1 <- workflow(
    occurrence = UKAnophelesPlumbeus,
    covariate = UKAir,
    process = Background(n = 70),
    model = LogisticRegression,
    output = SameTimePlaceMap,
    forceReproducible = TRUE
  )

  fName <- zoon:::WriteWorkflowMetadata(
    zoonWorkflow = work1,
    title = "This workflow",
    description = "Is a test",
    authors = "Tom August",
    categories = "testing",
    tags = c("code", "testing"),
    filename = tempfile()
  )

  expect_true(file.exists(fName),
              info = paste("Metadata file did not write correct,",
                           "file cannot be found"))

  fIn <- readLines(con = fName)

  fIn_expected <- c(
    "Title: This workflow",
    "Description: Is a test",
    "Authors: Tom August",
    "Categories: testing",
    "Tags: code",
    "Tags: testing",
    paste("Call: workflow(occurrence = UKAnophelesPlumbeus,",
          "covariate = UKAir, process = Background(n = 70),",
          "model = LogisticRegression, output = SameTimePlaceMap,",
          "forceReproducible = TRUE)")
  )

  expect_equal(fIn[c(1, 3:8)], fIn_expected)
})
