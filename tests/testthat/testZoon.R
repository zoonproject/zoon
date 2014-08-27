context('Most functions NOT including main workflow function.')

# CheckModStructure tests

test_that('CheckModStructure test', {
  expect_that(CheckModStructure('XXXX'), equals(list(module='XXXX')))
  expect_that(CheckModStructure(CheckModStructure('XXX')), equals(CheckModStructure('XXX')))
})



test_that('GetModule tests', {
  write('#test file for zoon package\n TestModule <- function(){z <- 2}', file = 'TestModule.R')
  file <- paste0(getwd(), '/TestModule.R')


  TestWorkflow <- function(){
    GetModule(file)
    return(class(TestModule))
  }

  TestModuleName <- function(){
    GetModule('NoProcess')
    return(class(NoProcess))
  }


  TestURL <- function(){
    GetModule('https://raw.githubusercontent.com/zoonproject/modules/master/R/NoProcess.R')
    return(class(NoProcess))
  }

  expect_error(GetModule('xxx'), 'Cannot find the module. Check the URL or check that the module is at github.com/zoonproject')
  expect_that(GetModule(file), equals('TestModule'))
  expect_that(TestWorkflow(), equals('function'))
  expect_that(GetModule('NoProcess'), equals('NoProcess'))
  expect_that(TestModuleName(), equals('function'))
  expect_that(GetModule('https://raw.githubusercontent.com/zoonproject/modules/master/R/NoProcess.R'), equals('NoProcess'))
  expect_that(TestURL(), equals('function'))
  expect_false(exists('NoProcess', env = globalenv()))
  expect_false(exists('TestModule', env = globalenv()))

  file.remove('TestModule.R')
})



