context('Most functions NOT including main workflow function.')


test_that('A test', {
  expect_that(1==1, is_true())
})


test_that('CheckModStructure test', {
  expect_that(CheckModStructure('XXXX'), equals(list(module='XXXX')))
})

test_that('delibfail', {
  expect_that(1==1, is_true())
})
