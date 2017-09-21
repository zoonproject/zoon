context("PasteAndDep")

test_that("PasteAndDep works", {
  a <- PasteAndDep(substitute("mod"))
  b <- PasteAndDep(substitute(mod(para = "pm")))
  c <- PasteAndDep(substitute(list("mod1", "mod2")))
  d <- PasteAndDep(substitute(list(mod1, "mod2")))
  e <- PasteAndDep(substitute(list(mod1, mod2)))
  f <- PasteAndDep(substitute(list(mod1(para = "pm"),
                                   "mod2")))
  g <- PasteAndDep(substitute(list(mod1(para = "pm"),
                                   mod2(para = "pm"))))
  h <- PasteAndDep(substitute(list(mod1(para = "pm", p2 = 2),
                                   mod2(para = "pm"))))
  i <- PasteAndDep(substitute(Chain(mod1(para = "m", p2 = 2),
                                    mod2(para = "pm"))))

  expect_true(
    all(vapply(list(a, b, c, d, e, f, g, h, i),
               inherits,
               "character",
               FUN.VALUE = FALSE))
  )

  expect_true(
    all(vapply(list(a, b, c, d, e, f, g, h, i),
               function (x) {
                 length(x) == 1
               },
               FUN.VALUE = FALSE))
  )
})
