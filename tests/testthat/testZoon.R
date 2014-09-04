context('Most functions NOT including main workflow function.')

# CheckModStructure tests

test_that('CheckModStructure works', {
  expect_that(CheckModStructure('XXXX'), equals(list(module='XXXX', paras=list())))
  expect_that(CheckModStructure(CheckModStructure('XXX')), equals(CheckModStructure('XXX')))
})


# CheckModList tests

test_that('CheckModList works.', {
	a <- 'mod'
	b <- ModuleOptions('mod', para='pm')
  c <- list('mod1', 'mod2')
	d <- list(ModuleOptions('mod', para='pm'), 'mod2')
	e <- list(ModuleOptions('mod1', para='pm'), ModuleOptions('mod2', para='pm'))

	expect_true(all(sapply(CheckModList(a), function(l) names(l) == c('module', 'paras'))))
	expect_true(all(sapply(CheckModList(b), function(l) names(l) == c('module', 'paras'))))
	expect_true(all(sapply(CheckModList(c), function(l) names(l) == c('module', 'paras'))))
	expect_true(all(sapply(CheckModList(d), function(l) names(l) == c('module', 'paras'))))
	expect_true(all(sapply(CheckModList(e), function(l) names(l) == c('module', 'paras'))))

	expect_equal(length(CheckModList(a)), 1)
	expect_equal(length(CheckModList(b)), 1)
	expect_equal(length(CheckModList(c)), 2)
	expect_equal(length(CheckModList(d)), 2)
	expect_equal(length(CheckModList(e)), 2)


})





test_that('Module Options tests', {
  
  options <- ModuleOptions('ANamedModule', para1 = 2, para2 = 'AValue')
  optList <- list(module = 'ANamedModule', paras=list(para1 = 2, para2 = 'AValue'))
  
  AModule <- function(){ x <- 2 }
  
  expect_identical(ModuleOptions('ANamedModule'), list(module='ANamedModule', paras=list()))
  expect_identical(options, optList)
  expect_error(ModuleOptions('ANamedModule', 2, 'AValue'))
  expect_error(ModuleOptions(234))
  expect_error(ModuleOptions(AModule))
})
