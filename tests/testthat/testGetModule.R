context("GetModule")

test_that("GetModule works", {
  # Get Module only accepts a module name
  #   Then gets module from namespace or zoon Repo onle

  NamespaceModule <- function() {
    return(UKAirRas)
  }

  # Have to do some weird messing here because tests are run in a special
  #   environment but GetModule looks in global and things.
  assign("NamespaceModule", NamespaceModule, env = .GlobalEnv)


  TestModuleName <- function() {
    GetModule("NoProcess", FALSE)
    return(class(NoProcess))
  }


  expect_error(GetModule("xxx", FALSE))
  expect_that(as.character(GetModule("NoProcess", FALSE)),
              equals("NoProcess"))
  expect_true("version" %in% names(attributes(GetModule("NoProcess", FALSE))))
  expect_equal(as.character(GetModule("NamespaceModule", FALSE)),
               "NamespaceModule")
  expect_equal(attr(GetModule("NamespaceModule", FALSE), "version"),
               "local copy")

  eval(GetModule("NamespaceModule", FALSE), env = .GlobalEnv)
  expect_true(exists("NamespaceModule", env = .GlobalEnv))
})
