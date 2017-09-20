context("FormatModuleList")

test_that("FormatModuleList works", {
  a <- FormatModuleList(substitute(module1))
  b <- FormatModuleList(substitute(module1(k = 2)))
  c <- FormatModuleList(substitute(module1(k = 2, l = "awd")))

  expect_true(all.equal(names(a), c("module", "paras")))
  expect_true(all.equal(names(b), c("module", "paras")))
  expect_true(all.equal(names(c), c("module", "paras")))

  expect_true(class(a$paras) == "list")
  expect_true(class(b$paras) == "list")
  expect_true(class(c$paras) == "list")

  expect_equal(a$paras, list())
  expect_equal(b$paras, list(k = 2))
  expect_equal(c$paras, list(k = 2, l = "awd"))
})
