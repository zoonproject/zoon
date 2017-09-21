context("testModules")

test_that("repo modules pass", {
  skip_on_cran()

  # Output
  basepath <- "https://raw.githubusercontent.com/zoonproject/modules/master/R/"
  
  expect_true(x <- zoon:::test_module(paste0(basepath,
                                             "PrintMap.R")))

  # Process PO
  expect_true(x <- zoon:::test_module(paste0(basepath,
                                             "OneHundredBackground.R")))

  # Process PA
  expect_true(x <- zoon:::test_module(paste0(basepath,
                                             "Crossvalidate.R")))

  # Model PA
  expect_true(x <- zoon:::test_module(paste0(basepath,
                                             "LogisticRegression.R")))

  # Model PB
  expect_true(x <- zoon:::test_module(paste0(basepath,
                                             "MaxLike.R")))

  # Covariate
  expect_true(x <- zoon:::test_module(paste0(basepath,
                                             "UKAir.R")))

  # Occurrence PO
  expect_true(x <- zoon:::test_module(paste0(basepath,
                                             "CarolinaWrenPO.R")))

  # Occurrence PA
  expect_true(x <- zoon:::test_module(paste0(basepath,
                                             "CWBZimbabwe.R")))
  
})
