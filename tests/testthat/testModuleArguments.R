context("ModuleArguments")

test_that("Check error if not found", {
  skip_on_cran()
  
  expect_error(
    ModuleArguments("ThisDoesNotExist"),
    "URL for module does not exist"
  )
  
})  

test_that("Check returns list with all the names", {
  x <- ModuleArguments("Background")
  
  arguments <- list(
    n = 100, 
    bias = NULL,
    seed = NULL
  )
  
  expect_that(x, is_identical_to(arguments))
})