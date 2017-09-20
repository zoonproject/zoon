context("LoadModule")

test_that("LoadModule works", {

  # Load module is only used to URLS and paths.

  # Create local module to test load.
  #   This will only work on unix. But I don't know how to test otherwise.
  #   LoadModule has to take the actual path,
  #   NOT e.g. fileName = '~/Test.R', LoadModule(fileName)
  #   So can't save to paste(getwd(), 'TestFile.R')
  write(
    "#test file for zoon package\n TestModule <- function(){z <- 2}",
    file = "~/TestModule.R"
  )


  TestWorkflow <- function() {
    LoadModule("~/TestModule.R")
    return(class(TestModule))
  }

  TestURL <- function() {
    LoadModule("https://raw.githubusercontent.com/zoonproject/modules/master/R/NoProcess.R")
    return(class(NoProcess))
  }

  expect_error(LoadModule("xxx"))
  expect_that(LoadModule("~/TestModule.R"), equals("TestModule"))
  expect_that(LoadModule("NoProcess"), equals("NoProcess"))
  expect_that(LoadModule("https://raw.githubusercontent.com/zoonproject/modules/master/R/NoProcess.R"), equals("NoProcess"))

  file.remove("~/TestModule.R")
})
