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

  # Have to do some weird messing here because tests are run in a special
  #   environment but GetModule looks in global and things.
  assign('NamespaceModule', NamespaceModule, env = .GlobalEnv)


  TestModuleName <- function(){
    GetModule('NoProcess', FALSE)
    return(class(NoProcess))
  }


  expect_error(GetModule('xxx', FALSE))
  expect_that(GetModule('NoProcess', FALSE), equals('NoProcess'))
  expect_equal(GetModule('NamespaceModule', FALSE), 'NamespaceModule')

  eval(GetModule('NamespaceModule', FALSE), env = .GlobalEnv)
  expect_true(exists('NamespaceModule', env = .GlobalEnv))
  
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


test_that('LapplyGetModule works correctly', {

  a <- list(module = 'UKAir', paras = list())
  b <- list(module = 'UKAir', paras = list())

  c <- LapplyGetModule(list(a,b), forceReproducible=FALSE)

  cexpect <- list(list(module = 'UKAir', paras = list(), func = 'UKAir'), 
               list(module = 'UKAir', paras = list(), func = 'UKAir'))

  expect_equal(c, cexpect)

  d <- list(module = 'UKAir', paras = list(a = 2, b = 'a'))

  e <- LapplyGetModule(list(a,d), forceReproducible=FALSE)

  eexpect <- list(list(module = 'UKAir', paras = list(), func = 'UKAir'), 
               list(module = 'UKAir', paras = list(a = 2, b = 'a'), func = 'UKAir'))

  expect_equal(e, eexpect)

  expect_true(exists('UKAir'))
})

test_that('RunModels function works correctly', {

  GetModule('LogisticRegression', forceReproducible = FALSE)
  e <- environment()

  # Test data with training and external validation data
  
  df <- data.frame(value = rep(c(0,1), 10),
                   type = rep(c('absence', 'presence'), 10), 
                   lon = 1:20, lat = 1:20,
                   fold = rep(c(1,0), each = 10),
                   cov1 = 1:20)


  x <- RunModels(df, 'LogisticRegression', list(), e)

  expect_equal(class(x$data), 'data.frame')
  expect_equal(class(x$model), c('glm', 'lm'))
  expect_true('predictions' %in% names(x$data))
  # As no crossvalidation, training data should not have predictions
  expect_true(all(is.na(x$data[x$data['fold'] == 1,'predictions'])))

  # The final model should be trained on all data.
  expect_true(length(x$model$y) == 20)




  # Test data with crossvalidation data
  
  df2 <- data.frame(value = rep(c(0,1), 10),
                   type = rep(c('absence', 'presence'), 10), 
                   lon = 1:20, lat = 1:20,
                   fold = rep(c(1,2), each = 10),
                   cov1 = c(1:10, 1:10))


  x2 <- RunModels(df2, 'LogisticRegression', list(), e)

  expect_equal(class(x2$data), 'data.frame')
  expect_equal(class(x2$model), c('glm', 'lm'))
  expect_true('predictions' %in% names(x$data))

  # As crossvalidation, all data should have predictions
  expect_true(all(!is.na(x2$data[,'predictions'])))

  # The final model should be trained on all data.
  expect_true(length(x2$model$y) == 20)

  # Because cov1 is artificially replicated, predictions should be equal
  expect_true(all.equal(x2$data[1:10,'predictions'], 
                        x2$data[11:20,'predictions']))



})

