
context('Test workflow function.')

test_that('simple, package data workflow works.', {
  work1 <- workflow(occurMod = 'UKAnophelesPlumbeus',
                 covarMod = 'UKAir',
                 procMod = 'OneHundredBackground',
                 modelMod = 'LogisticRegression',
                 outMod = 'SameTimePlaceMap')

  expect_true(exists('work1'))
  expect_equal(names(work1), c('occurrence.output', 'covariate.output', 'process.output', 'model.output', 'output.output'))
  expect_equal(dim(work1$occurrence.output[[1]]), c(199, 4))
  expect_is(work1$covariate.output[[1]], 'RasterLayer')
  expect_equal(dim(work1$covariate.output[[1]]), c(9,9,1))
  expect_equal(names(work1$process.output[[1]]), c('value', 'type',  'lon',   'lat',   'layer'))
  expect_equal(dim(work1$process.output[[1]]),  c(280, 5))
  expect_is(work1$model.output[[1]], c('glm', 'lm'))
  expect_is(work1$output[[1]], 'RasterLayer')
  

})

test_that('modules downloading data work', {

  work2 <- workflow(occurMod = ModuleOptions('SpOcc', species = 'Anopheles plumbeus', extent = c(-20, 20, 45, 65)),
                   covarMod = ModuleOptions('NCEP', variables = c('air', 'rhum'), extent = c(-20,20,45,65)),
                   procMod = 'OneHundredBackground',
                   modelMod = 'RandomForest',
                   outMod = 'SameTimePlaceMap')
  
  expect_true(exists('work2'))
  expect_equal(names(work2), c('occurrence.output', 'covariate.output', 'process.output', 'model.output', 'output.output'))
  expect_is(work2$occurrence.output[[1]], 'data.frame')
  expect_equal(names(work2$occurrence.output[[1]]), c('longitude', 'latitude', 'value', 'type'))
  expect_true(all(work2$occurrence.output[[1]][,'longitude'] < 20))
  expect_true(all(work2$occurrence.output[[1]][,'longitude'] > -20))
  expect_true(all(work2$occurrence.output[[1]][,'latitude'] < 65))
  expect_true(all(work2$occurrence.output[[1]][,'latitude'] > 45))
  expect_true(all(work2$occurrence.output[[1]][,'type']=='presence'))
  expect_is(work2$covariate.output[[1]], 'RasterStack')
  expect_is(work2$model.output[[1]], 'randomForest')
  expect_is(work2$output[[1]], 'RasterLayer')
})

test_that('collecting modules with names and urls is equivelent.', {
  set.seed(1)
  workNames <- workflow(occurMod = 'UKAnophelesPlumbeus',
                 covarMod = 'UKAir',
                 procMod = 'OneHundredBackground',
                 modelMod = 'LogisticRegression',
                 outMod = 'SameTimePlaceMap')
  set.seed(1)
  workURLs <- workflow(occurMod = 'https://raw.githubusercontent.com/zoonproject/modules/master/R/Occurrence/UKAnophelesPlumbeus.R',
                 covarMod = 'https://raw.githubusercontent.com/zoonproject/modules/master/R/Covariate/UKAir.R',
                 procMod = 'https://raw.githubusercontent.com/zoonproject/modules/master/R/Process/OneHundredBackground.R',
                 modelMod = 'https://raw.githubusercontent.com/zoonproject/modules/master/R/Model/LogisticRegression.R',
                 outMod = 'https://raw.githubusercontent.com/zoonproject/modules/master/R/Output/SameTimePlaceMap.R')

expect_equal(workNames, workURLs)

})

test_that('Workflows with lists of modules work.', {
  # Would like to remove some of the slow online database modules from here.
  workOccurList <- workflow(occurMod = list('UKAnophelesPlumbeus', 
                        ModuleOptions('SpOcc', species = 'Anopheles plumbeus', 
                          extent = c(-20, 20, 45, 65))),
                       covarMod = 'UKAir',
                       procMod = 'OneHundredBackground',
                       modelMod = 'LogisticRegression',
                       outMod = 'SameTimePlaceMap')

  workCovarList <- workflow(occurMod = 'UKAnophelesPlumbeus',
                     covarMod = list('UKAir', 'UKAir'),
                     procMod = 'OneHundredBackground',
                     modelMod = 'LogisticRegression',
                     outMod = 'SameTimePlaceMap')

  # There's only 1 appropriate process module at the moment!
  workProcessList <- workflow(occurMod = 'UKAnophelesPlumbeus',
                       covarMod = 'UKAir',
                       procMod = list('OneHundredBackground','OneHundredBackground'),
                       modelMod = 'LogisticRegression',
                       outMod = 'SameTimePlaceMap')

  workModelList <- workflow(occurMod = 'UKAnophelesPlumbeus',
                     covarMod = 'UKAir',
                     procMod = 'OneHundredBackground',
                     modelMod = list('LogisticRegression', 'RandomForest'),
                     outMod = 'SameTimePlaceMap')

  workOutputList <- workflow(occurMod = 'UKAnophelesPlumbeus',
                     covarMod = 'UKAir',
                     procMod = 'OneHundredBackground',
                     modelMod = 'LogisticRegression',
                     outMod = list('SameTimePlaceMap', 'SameTimePlaceMap'))

  expect_equivalent(sapply(workOccurList, length), c(2,1,2,2,2))
  expect_equivalent(sapply(workCovarList, length), c(1,2,2,2,2))
  expect_equivalent(sapply(workProcessList, length), c(1,1,2,2,2))
  expect_equivalent(sapply(workModelList, length), c(1,1,1,2,2))
  expect_equivalent(sapply(workOutputList, length), c(1,1,1,1,2))

  occurClasses <- unlist(lapply(workOccurList, function(x) sapply(x, class)))
  covarClasses <- unlist(lapply(workCovarList, function(x) sapply(x, class)))
  processClasses <- unlist(lapply(workProcessList, function(x) sapply(x, class)))
  modelClasses <- unlist(lapply(workModelList, function(x) sapply(x, class)))
  outputClasses <- unlist(lapply(workOutputList, function(x) sapply(x, class)))

  expect_equivalent(occurClasses, c('data.frame','data.frame','RasterLayer','data.frame',
    'data.frame','glm','lm','glm','lm','RasterLayer','RasterLayer'))
  expect_equivalent(covarClasses, c('data.frame','RasterLayer','RasterLayer','data.frame',
    'data.frame','glm','lm','glm','lm','RasterLayer','RasterLayer'))
  expect_equivalent(processClasses, c('data.frame','RasterLayer','data.frame',
    'data.frame','glm','lm','glm','lm','RasterLayer','RasterLayer'))
  expect_equivalent(modelClasses, c('data.frame','RasterLayer','data.frame',
    'glm','lm','randomForest.formula','randomForest','RasterLayer','RasterLayer'))
  expect_equivalent(outputClasses, c('data.frame','RasterLayer','data.frame',
    'glm','lm','RasterLayer','RasterLayer'))

})

test_that('only one set of multiple lists allowed.', {
  fnc1 <- function(){
    x <- workflow(occurMod = list('UKAnophelesPlumbeus',
                    'UKAnophelesPlumbeus'),
           covarMod = list('UKAir', 'UKAir'),
           procMod = 'OneHundredBackground',
           modelMod = 'LogisticRegression',
           outMod = 'SameTimePlaceMap')
  }

fnc2 <- function(){
    x <- workflow(occurMod = 'UKAnophelesPlumbeus',
           covarMod = list('UKAir', 'UKAir'),
           procMod = list('OneHundredBackground','OneHundredBackground'),
           modelMod = 'LogisticRegression',
           outMod = 'SameTimePlaceMap')
  }

fnc3 <- function(){
    x <- workflow(occurMod = 'UKAnophelesPlumbeus',
           covarMod = 'UKAir',
           procMod = 'OneHundredBackground',
           modelMod = list('LogisticRegression','LogisticRegression'),
           outMod = list('SameTimePlaceMap', 'SameTimePlaceMap'))
  }

  expect_error(fnc1())
  expect_error(fnc2())
  expect_error(fnc3())
  
})


