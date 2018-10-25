context("CheckModList")


test_that("CheckModList works.", {
  a <- substitute("mod")
  b <- substitute(mod(para = "pm"))
  c <- substitute(list("mod1", "mod2"))
  d <- substitute(list(mod1, "mod2"))
  e <- substitute(list(mod1, mod2))
  f <- substitute(list(mod1(para = "pm"), "mod2"))
  g <- substitute(list(mod1(para = "pm"), mod2(para = "pm")))
  h <- substitute(list(mod1(para = "pm", p2 = 2), mod2(para = "pm")))
  i <- substitute(Chain(mod1(para = "m", p2 = 2), mod2(para = "pm")))


  # Deal with occurrence = 'module1(k=2)',

  testNames <- function(l) names(l) == c("module", "paras")

  eg <- c(FALSE, FALSE)
  expect_true(all(vapply(CheckModList(a), testNames, eg)))
  expect_true(all(vapply(CheckModList(b), testNames, eg)))
  expect_true(all(vapply(CheckModList(c), testNames, eg)))
  expect_true(all(vapply(CheckModList(d), testNames, eg)))
  expect_true(all(vapply(CheckModList(e), testNames, eg)))
  expect_true(all(vapply(CheckModList(f), testNames, eg)))
  expect_true(all(vapply(CheckModList(g), testNames, eg)))
  expect_true(all(vapply(CheckModList(h), testNames, eg)))
  expect_true(all(vapply(CheckModList(i), testNames, eg)))

  expect_equal(length(CheckModList(a)), 1)
  expect_equal(length(CheckModList(b)), 1)
  expect_equal(length(CheckModList(c)), 2)
  expect_equal(length(CheckModList(d)), 2)
  expect_equal(length(CheckModList(e)), 2)
  expect_equal(length(CheckModList(f)), 2)
  expect_equal(length(CheckModList(g)), 2)
  expect_equal(length(CheckModList(h)), 2)

  expect_true(identical(attr(CheckModList(i), "chain"), TRUE))
  expect_false(identical(attr(CheckModList(a), "chain"), TRUE))
  expect_false(identical(attr(CheckModList(b), "chain"), TRUE))
  expect_false(identical(attr(CheckModList(c), "chain"), TRUE))
})

test_that("CheckModList errors correctly", {
  a <- substitute('mod(para="pm")')
  b <- substitute("list('mod1', 'mod2')")
  grepl("[^\' | ^\"]", a) & grepl("[( | )]", a)

  expect_error(CheckModList(a),
               "If specifying module arguments please use the form")
  expect_error(CheckModList(b),
               "If specifying module arguments please use the form")
})
