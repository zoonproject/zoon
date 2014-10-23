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

  # The final model should be trained on non external validation data.
  expect_true(length(x$model$y) == 10)




  # Test data with crossvalidation data
  
  df2 <- data.frame(value = rep(c(0,1), 10),
                   type = rep(c('absence', 'presence'), 10), 
                   lon = 1:20, lat = 1:20,
                   fold = rep(c(1,2), each = 10),
                   cov1 = c(1:10, 1:10))


  x2 <- RunModels(df2, 'LogisticRegression', list(), e)

  expect_equal(class(x2$data), 'data.frame')
  expect_equal(class(x2$model), c('glm', 'lm'))
  expect_true('predictions' %in% names(x2$data))

  # As crossvalidation, all data should have predictions
  expect_true(all(!is.na(x2$data[,'predictions'])))

  # The final model should be trained on all data.
  expect_true(length(x2$model$y) == 20)

  # Because cov1 is artificially replicated, predictions should be equal
  expect_true(all.equal(x2$data[1:10,'predictions'], 
                        x2$data[11:20,'predictions']))


  # Mix of external and cross validation
  df3 <- data.frame(value = rep(c(0,1), 10),
                   type = rep(c('absence', 'presence'), 10), 
                   lon = 1:20, lat = 1:20,
                   fold = c(rep(c(1,2), each = 5), rep(0,10)),
                   cov1 = c(1:10, 1:10))


  x3 <- RunModels(df3, 'LogisticRegression', list(), e)

  expect_equal(class(x3$data), 'data.frame')
  expect_equal(class(x3$model), c('glm', 'lm'))
  expect_true('predictions' %in% names(x3$data))

  # All data should have predictions, cross validated data from CV and
  #   and external validation from full model
  expect_true(all(!is.na(x3$data[,'predictions'])))

  # The final model should be trained on half data.
  expect_true(length(x3$model$y) == 10)


})

test_that('SplitArgs works', {
#' A function that takes a string (from workflow$call) and splits it into a
#'   module name and it's arguments.
#'
#'@param string A string of the form "moduleName" or 
#'  "moduleName(parameter = 2, parameter2 = 3)"

  a <- SplitArgs("moduleName(parameter = 2, parameter2 = 3)")
  b <- SplitArgs("moduleName")

  # Interesting to note that SplitArgs doesn't deal with numerics at all. 
  #   Probably needs looking into.
  expect_equal(a, list(module = 'moduleName', paras = list(parameter = '2', parameter2 = '3')))
  expect_equal(b, list(module = 'moduleName', paras = list()))
})



test_that('FormatModuleList works', {
  
  a <- FormatModuleList(substitute(module1))
  b <- FormatModuleList(substitute(module1(k=2)))
  c <- FormatModuleList(substitute(module1(k=2, l='awd')))
  
  expect_true(all.equal(names(a), c('module', 'paras')))
  expect_true(all.equal(names(b), c('module', 'paras')))
  expect_true(all.equal(names(c), c('module', 'paras')))

  expect_true(class(a$paras) == 'list')
  expect_true(class(b$paras) == 'list')
  expect_true(class(c$paras) == 'list')

  expect_equal(a$paras, list())
  expect_equal(b$paras, list(k=2))
  expect_equal(c$paras, list(k=2, l='awd'))
})


test_that('ExtractAndCombData works.', {
  
  x <- ExtractAndCombData(head(AplumbeusOcc), UKAirRas)
  
  expect_true(inherits(x, 'list'))
  expect_true(all.equal(names(x), c('df', 'ras')))
  expect_true(all.equal(dim(x$df), c(6,6)))

  # Give an error if trying to use occurrence data outside raster extent
  outOfExtentData <- head(AplumbeusOcc)
  outOfExtentData$longitude <- -20

  expect_error(ExtractAndCombData(outOfExtentData, UKAirRas))

})


test_that('SortArgs works.', {
  x <- SortArgs(PasteAndDep(substitute(UKAnophelesPlumbeus)), 
                PasteAndDep(substitute(UKAir)), 
                PasteAndDep(substitute(OneHundredBackground)), 
                PasteAndDep(substitute(LogisticRegression)),
                PasteAndDep(substitute(SameTimePlaceMap)),
                TRUE)

  expect_equal(x,
    "workflow(occurrence = UKAnophelesPlumbeus, covariate = UKAir, process = OneHundredBackground, model = LogisticRegression, output = SameTimePlaceMap, forceReproducible = TRUE)")

  # Check the call is runeable
  w <- eval(parse(text = x))
  expect_true(inherits(w, 'zoonWorkflow'))
  expect_false(any(sapply(w, is.null)))


  # More complex syntax
  y <- SortArgs(PasteAndDep(substitute(UKAnophelesPlumbeus)), 
                PasteAndDep(substitute('UKAir')), 
                PasteAndDep(substitute(BackgroundAndCrossvalid(k=2))), 
                PasteAndDep(substitute(list(LogisticRegression, LogisticRegression))),
                PasteAndDep(substitute(Chain(SameTimePlaceMap, SameTimePlaceMap))),
                TRUE)

  expect_true(length(y) == 1)
  expect_true(inherits(y, 'character'))

  # Check the call is runeable
  w2 <- eval(parse(text = y))
  expect_true(inherits(w2, 'zoonWorkflow'))
  expect_false(any(sapply(w2, is.null)))

})



test_that('SplitCall works', {
  # This function is only ever used on calls from a workflow object, so we can use
  #   SortArgs to simulate the possible things we might get.


  call1 <- SortArgs(PasteAndDep(substitute(UKAnophelesPlumbeus)), 
                PasteAndDep(substitute(UKAir)), 
                PasteAndDep(substitute(OneHundredBackground)), 
                PasteAndDep(substitute(LogisticRegression)),
                PasteAndDep(substitute(SameTimePlaceMap)),
                TRUE)


  split1 <- SplitCall(call1)

  expect_true(inherits(split1, 'character'))
  expect_equal(length(split1), 6)


  # Test argument inputs, lists, chains and character inputs.
  call2 <- SortArgs(PasteAndDep(substitute(UKAnophelesPlumbeus)), 
                PasteAndDep(substitute('UKAir')), 
                PasteAndDep(substitute(BackgroundAndCrossvalid(k=2))), 
                PasteAndDep(substitute(list(LogisticRegression, LogisticRegression))),
                PasteAndDep(substitute(Chain(SameTimePlaceMap, SameTimePlaceMap))),
                TRUE)

  split2 <- SplitCall(call2)

  expect_true(inherits(split2, 'character'))
  expect_equal(length(split2), 6)


  
  # Test List of arguments, list of characters
  call3 <- SortArgs(PasteAndDep(substitute(UKAnophelesPlumbeus(k='awd', v = 2))), 
                PasteAndDep(substitute(Chain('UKAir', 'UKAir'))), 
                PasteAndDep(substitute(list(BackgroundAndCrossvalid(k=2), BackgroundAndCrossvalid(k=2,l=3)))), 
                PasteAndDep(substitute(list(LogisticRegression, LogisticRegression))),
                PasteAndDep(substitute(Chain(SameTimePlaceMap(l=2), SameTimePlaceMap(l=2,k=3,r='23')))),
                TRUE)

  split3 <- SplitCall(call3)

  expect_true(inherits(split3, 'character'))
  expect_equal(length(split3), 6)

})


test_that('ErrorAndSave works.', {

  # As this throws errors it's quite hard to test.
  #   To properly test that tmpZoonWorkflow is returned properly is a pain as well
  #   Probably easier to test this thoroughly in test-wholeWorkflow by breaking workflows.

  expect_error(ErrorAndSave('esv', 1, environment()), 'Stopping workflow due to error in')

  expect_true(exists('tmpZoonWorkflow'))
})


test_that('PasteAndDep works', {
  a <- PasteAndDep(substitute('mod'))
	b <- PasteAndDep(substitute(mod(para='pm')))
  c <- PasteAndDep(substitute(list('mod1', 'mod2')))
  d <- PasteAndDep(substitute(list(mod1, 'mod2')))
  e <- PasteAndDep(substitute(list(mod1, mod2)))
	f <- PasteAndDep(substitute(list(mod1(para='pm'), 'mod2')))
	g <- PasteAndDep(substitute(list(mod1(para='pm'), mod2(para='pm'))))
  h <- PasteAndDep(substitute(list(mod1(para='pm', p2 = 2), mod2(para='pm'))))
  i <- PasteAndDep(substitute(Chain(mod1(para='m', p2 = 2), mod2(para='pm'))))

  expect_true(
    all(sapply(list(a,b,c,d,e,f,g,h,i), function(x) inherits(x, 'character')))
  )
  
  expect_true(
    all(sapply(list(a,b,c,d,e,f,g,h,i), function(x) length(x) == 1))
  )
  

})
