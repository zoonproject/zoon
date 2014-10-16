context('Most functions NOT including main workflow function.')

# CheckModList tests

test_that('CheckModList works.', {
	a <- substitute('mod')
	b <- substitute(mod(para='pm'))
  c <- substitute(list('mod1', 'mod2'))
  d <- substitute(list(mod1, 'mod2'))
  e <- substitute(list(mod1, mod2))
	f <- substitute(list(mod1(para='pm'), 'mod2'))
	g <- substitute(list(mod1(para='pm'), mod2(para='pm')))
  h <- substitute(list(mod1(para='pm', p2 = 2), mod2(para='pm')))
  i <- substitute(Chain(mod1(para='m', p2 = 2), mod2(para='pm')))


  # Deal with occurrence = 'module1(k=2)',

  testNames <- function(l) names(l) == c('module', 'paras')

	expect_true(all(sapply(CheckModList(a), testNames)))
	expect_true(all(sapply(CheckModList(b), testNames)))
	expect_true(all(sapply(CheckModList(c), testNames)))
	expect_true(all(sapply(CheckModList(d), testNames)))     
	expect_true(all(sapply(CheckModList(e), testNames)))
	expect_true(all(sapply(CheckModList(f), testNames)))
	expect_true(all(sapply(CheckModList(g), testNames)))
	expect_true(all(sapply(CheckModList(h), testNames)))
	expect_true(all(sapply(CheckModList(i), testNames)))

	expect_equal(length(CheckModList(a)), 1)
	expect_equal(length(CheckModList(b)), 1)
	expect_equal(length(CheckModList(c)), 2)
	expect_equal(length(CheckModList(d)), 2)
	expect_equal(length(CheckModList(e)), 2)	
	expect_equal(length(CheckModList(f)), 2)
	expect_equal(length(CheckModList(g)), 2)
	expect_equal(length(CheckModList(h)), 2)

  expect_true(identical(attr(CheckModList(i), 'chain'), TRUE))
  expect_false(identical(attr(CheckModList(a), 'chain'), TRUE))
  expect_false(identical(attr(CheckModList(b), 'chain'), TRUE))
  expect_false(identical(attr(CheckModList(c), 'chain'), TRUE))

})



test_that('GetModule works', {
  # Get Module only accepts a module name
  #   Then gets module from namespace or zoon Repo onle

  NamespaceModule <- function(){
      return(UKAirRas)
    }


  TestModuleName <- function(){
    GetModule('NoProcess', FALSE)
    return(class(NoProcess))
  }


  expect_error(GetModule('xxx', FALSE))
  expect_that(GetModule('NoProcess', FALSE), equals('NoProcess'))

  
})



test_that('LoadModule works', {

  # Load module is only used to URLS and paths.

  # Create local module to test load.
  #   This will only work on unix. But I don't know how to test otherwise.
  #   LoadModule has to take the actual path, 
  #   NOT e.g. fileName = '~/Test.R', LoadModule(fileName)
  #   So can't save to paste(getwd(), 'TestFile.R')
  write('#test file for zoon package\n TestModule <- function(){z <- 2}', 
    file = '~/TestModule.R')


  TestWorkflow <- function(){
    LoadModule(file)
    return(class(TestModule))
  }

  TestURL <- function(){
    LoadModule('https://raw.githubusercontent.com/zoonproject/modules/master/R/NoProcess.R')
    return(class(NoProcess))
  }

  expect_error(LoadModule('xxx'))
  expect_that(LoadModule('~/TestModule.R'), equals('TestModule'))
  expect_that(LoadModule('NoProcess'), equals('NoProcess'))
  expect_that(LoadModule('https://raw.githubusercontent.com/zoonproject/modules/master/R/NoProcess.R'), equals('NoProcess'))
  
  file.remove('~/TestModule.R')
})


test_that('SplitCall works correctly', {

})


