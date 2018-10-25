context("ModuleHelp")

directory <- tempdir()

test_that("ModuleHelp errors", {
  if (!capabilities("libcurl"))
    skip("skipping as libcurl not supported")

  expect_error(
    ModuleHelp(c(one, two)),
    "module must be a character of length 1"
  )

  expect_error(
    ModuleHelp("NoProce1s"),
    "Can't find 'NoProce1s'. Did you mean"
  )
})

test_that("Help as text", {
  org <- getOption("help_type")
  options(help_type = "text")

  capture.output(
    ModuleHelp("NoProcess"),
    file = file.path(directory, "help_text.txt")
  )

  expect_true(file.exists(file.path(directory, "help_text.txt")))
  unlink(x = file.path(directory, "help_text.txt"))

  options(help_type = org)
})

test_that("Help as NULL", {
  org <- getOption("help_type")
  options(help_type = NULL)

  capture.output(
    ModuleHelp("NoProcess"),
    file = file.path(directory, "help_null.txt")
  )

  expect_true(file.exists(file.path(directory, "help_null.txt")))
  unlink(x = file.path(directory, "help_null.txt"))

  options(help_type = org)
})

test_that("Help as html", {
  org <- getOption("help_type")
  options(help_type = "html")
  browser <- getOption("viewer")
  options(viewer = NULL)

  expect_warning(
    capture.output(
      ModuleHelp("NoProcess"),
      file = file.path(directory, "help.html")
    ),
    "^To display html help"
  )

  expect_true(file.exists(file.path(directory, "help.html")))
  unlink(x = file.path(directory, "help.html"))

  options(viewer = browser)

  capture.output(
    ModuleHelp("NoProcess"),
    file = file.path(directory, "help.html")
  )

  expect_true(file.exists(file.path(directory, "help.html")))
  unlink(x = file.path(directory, "help.html"))

  options(help_type = org)
})


test_that("Help as pdf", {
  org <- getOption("help_type")
  options(help_type = "pdf")

  expect_warning(
    capture.output(
      ModuleHelp("NoProcess"),
      file = file.path(directory, "help.txt")
    ),
    "^pdf help files"
  )

  expect_true(file.exists(file.path(directory, "help.txt")))
  unlink(x = file.path(directory, "help.txt"))

  options(help_type = org)



  # Some modules with no close matches
  expect_error(ModuleHelp("zzzzz123232"),
               "or any modules with closely matching names")


  # Module with 1 close matches
  expect_error(ModuleHelp("NoProcesss"),
               "Can't find 'NoProcesss'. Did you mean")

  # Module with 2 close matches
  expect_error(ModuleHelp("Crissvalid"),
               "Can't find 'Crissvalid'. Did you mean one of")
})
