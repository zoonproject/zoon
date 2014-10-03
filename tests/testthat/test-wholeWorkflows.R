
context('Test workflow function.')

test_that('simple, package data workflow works.', {
  work1 <- workflow(occurrence = UKAnophelesPlumbeus,
                 covariate = UKAir,
                 process = OneHundredBackground,
                 model = LogisticRegression,
                 output = SameTimePlaceMap)

  expect_true(exists('work1'))
  expect_equal(names(work1), 
    c('occurrence.output', 'covariate.output', 'process.output', 
      'model.output', 'report', 'call'))
  expect_equal(dim(work1$occurrence.output[[1]]), c(188,5))
  expect_is(work1$covariate.output[[1]], 'RasterLayer')
  expect_equal(dim(work1$covariate.output[[1]]), c(9,9,1))
  expect_equal(names(work1$process.output[[1]]$df), 
    c('value', 'type', 'fold', 'longitude',   'latitude',   'layer'))
  expect_equal(dim(work1$process.output[[1]][[1]]),  c(269, 6))
  expect_is((work1$model.output[[1]])$model, c('glm', 'lm'))
  expect_is((work1$model.output[[1]])$data, c('data.frame'))
  expect_is(work1$report[[1]], 'RasterLayer')
  

})


test_that('Check basic quoted workflow.', {
  work1 <- workflow(occurrence = 'UKAnophelesPlumbeus',
                 covariate = 'UKAir',
                 process = 'OneHundredBackground',
                 model = 'LogisticRegression',
                 output = 'SameTimePlaceMap')

  expect_true(exists('work1'))
  expect_equal(names(work1), 
    c('occurrence.output', 'covariate.output', 'process.output', 
      'model.output', 'report', 'call'))
  expect_equal(dim(work1$occurrence.output[[1]]), c(188,5))
  expect_is(work1$covariate.output[[1]], 'RasterLayer')
  expect_equal(dim(work1$covariate.output[[1]]), c(9,9,1))
  expect_equal(names(work1$process.output[[1]]$df), 
    c('value', 'type', 'fold', 'longitude',   'latitude',   'layer'))
  expect_equal(dim(work1$process.output[[1]][[1]]),  c(269, 6))
  expect_is((work1$model.output[[1]])$model, c('glm', 'lm'))
  expect_is((work1$model.output[[1]])$data, c('data.frame'))
  expect_is(work1$report[[1]], 'RasterLayer')
  

})



test_that('modules downloading data work', {

  work2 <- workflow(occurrence = SpOcc(species = 'Anopheles plumbeus', extent = c(-20, 20, 45, 65)),
                    covariate = UKAir,
                    process = OneHundredBackground,
                    model = RandomForest, 
                    output = SameTimePlaceMap)
  
  expect_true(exists('work2'))
  expect_equal(names(work2), 
    c('occurrence.output', 'covariate.output', 'process.output', 
      'model.output', 'report', 'call'))
  expect_is(work2$occurrence.output[[1]], 'data.frame')
  expect_equal(names(work2$occurrence.output[[1]]), c('longitude', 'latitude', 'value', 'type', 'fold'))
  expect_true(all(work2$occurrence.output[[1]][,'longitude'] < 20))
  expect_true(all(work2$occurrence.output[[1]][,'longitude'] > -20))
  expect_true(all(work2$occurrence.output[[1]][,'latitude'] < 65))
  expect_true(all(work2$occurrence.output[[1]][,'latitude'] > 45))
  expect_true(all(work2$occurrence.output[[1]][,'type']=='presence'))
  expect_is(work2$covariate.output[[1]], 'RasterLayer')
  expect_is((work2$model.output[[1]])$model, 'randomForest')
  expect_is(work2$report[[1]], 'RasterLayer')
})

test_that('collecting modules with names and urls is equivelent.', {
  set.seed(1)
  workNames <- workflow(occurrence = UKAnophelesPlumbeus,
                         covariate = UKAir,
                         process = OneHundredBackground,
                         model = LogisticRegression,
                         output = SameTimePlaceMap)
  set.seed(1)
  workURLs <- workflow(occurrence = 'https://raw.githubusercontent.com/zoonproject/modules/master/R/UKAnophelesPlumbeus.R',
                 covariate = 'https://raw.githubusercontent.com/zoonproject/modules/master/R/UKAir.R',
                 process = 'https://raw.githubusercontent.com/zoonproject/modules/master/R/OneHundredBackground.R',
                 model = 'https://raw.githubusercontent.com/zoonproject/modules/master/R/LogisticRegression.R',
                 output = 'https://raw.githubusercontent.com/zoonproject/modules/master/R/SameTimePlaceMap.R')

expect_equal(workNames[[1]], workURLs[[1]])
expect_equal(workNames[[2]], workURLs[[2]])
expect_equal(workNames[[3]], workURLs[[3]])
expect_equal(workNames[[4]], workURLs[[4]])
expect_equal(workNames[[5]], workURLs[[5]])

})

test_that('Workflows with lists of modules work.', {
  
  # Would like to remove some of the slow online database modules from here.
  # In fact I don't think the would pass cran.
  workOccurList <- workflow(occurrence = list(UKAnophelesPlumbeus, 
                        SpOcc(species = 'Anopheles plumbeus', 
                          extent = c(-20, 20, 45, 65))),
                        covariate = UKAir,
                        process = OneHundredBackground,
                        model = LogisticRegression,
                        output = SameTimePlaceMap)

  workCovarList <- workflow(occurrence = UKAnophelesPlumbeus,
                     covariate = list(UKAir, UKAir),
                     process = OneHundredBackground,
                     model = LogisticRegression,
                     output = SameTimePlaceMap)

  # There's only 1 appropriate process module at the moment!
  workProcessList <- workflow(occurrence = UKAnophelesPlumbeus,
                       covariate = UKAir,
                       process = list(OneHundredBackground, OneHundredBackground),
                       model = LogisticRegression,
                       output = SameTimePlaceMap)

  workModelList <- workflow(occurrence = UKAnophelesPlumbeus,
                     covariate = UKAir,
                     process = OneHundredBackground,
                     model = list(LogisticRegression, RandomForest),
                     output = SameTimePlaceMap)

  workOutputList <- workflow(occurrence = UKAnophelesPlumbeus,
                     covariate = UKAir,
                     process = OneHundredBackground,
                     model = LogisticRegression,
                     output = list(SameTimePlaceMap, SameTimePlaceMap))

  expect_equivalent(sapply(workOccurList, length), c(2, 1, 2, 2, 2, 1))
  expect_equivalent(sapply(workCovarList, length), c(1, 2, 2, 2, 2, 1))
  expect_equivalent(sapply(workProcessList, length), c(1, 1, 2, 2, 2, 1))
  expect_equivalent(sapply(workModelList, length), c(1, 1, 1, 2, 2, 1))
  expect_equivalent(sapply(workOutputList, length), c(1, 1, 1, 1, 2, 1))

  occurClasses <- unlist(lapply(workOccurList, function(x) sapply(x, class)))
  covarClasses <- unlist(lapply(workCovarList, function(x) sapply(x, class)))
  processClasses <- unlist(lapply(workProcessList, function(x) sapply(x, class)))
  modelClasses <- unlist(lapply(workModelList, function(x) sapply(x, class)))
  outputClasses <- unlist(lapply(workOutputList, function(x) sapply(x, class)))

  expect_equivalent(occurClasses, c('data.frame','data.frame','RasterLayer','list',
    'list','list','list','RasterLayer','RasterLayer', 'character'))
  expect_equivalent(covarClasses, c('data.frame','RasterLayer','RasterLayer','list',
    'list','list','list','RasterLayer','RasterLayer', 'character'))
  expect_equivalent(processClasses, c('data.frame','RasterLayer','list',
    'list','list','list','RasterLayer','RasterLayer', 'character'))
  expect_equivalent(modelClasses, c('data.frame','RasterLayer','list',
    'list','list','RasterLayer','RasterLayer', 'character'))
  expect_equivalent(outputClasses, c('data.frame','RasterLayer','list',
    'list','RasterLayer','RasterLayer', 'character'))

})

test_that('only one set of multiple lists allowed.', {
  fnc1 <- function(){
    x <- workflow(occurrence = list(UKAnophelesPlumbeus,
                    UKAnophelesPlumbeus),
           covariate = list(UKAir, UKAir),
           process = OneHundredBackground,
           model = LogisticRegression,
           output = SameTimePlaceMap)
  }

fnc2 <- function(){
    x <- workflow(occurrence = UKAnophelesPlumbeus,
           covariate = list(UKAir, UKAir),
           process = list(OneHundredBackground,OneHundredBackground),
           model = LogisticRegression,
           output = SameTimePlaceMap)
  }

fnc3 <- function(){
    x <- workflow(occurrence = UKAnophelesPlumbeus,
           covariate = UKAir,
           process = OneHundredBackground,
           model = list(LogisticRegression,LogisticRegression),
           output = list(SameTimePlaceMap, SameTimePlaceMap))
  }

  expect_error(fnc1())
  expect_error(fnc2())
  expect_error(fnc3())
  
})



test_that('simple, crossvalidation workflow works.', {

  workCross <- workflow(occurrence = UKAnophelesPlumbeus,
                 covariate = UKAir,
                 process = BackgroundAndCrossvalid,
                 model = LogisticRegression,
                 output = SameTimePlaceMap)

  expect_true(exists('workCross'))
  expect_equal(names(workCross), c('occurrence.output', 'covariate.output',
    'process.output', 'model.output', 'report', 'call'))
  expect_equal(dim(workCross$occurrence.output[[1]]), c(188, 5))
  expect_is(workCross$covariate.output[[1]], 'RasterLayer')
  expect_equal(dim(workCross$covariate.output[[1]]), c(9,9,1))
  expect_equal(names(workCross$process.output[[1]]$df), 
    c('value', 'type', 'fold', 'longitude', 'latitude', 'layer'))
  expect_equal(dim(workCross$process.output[[1]]$df),  c(269, 6))
  expect_is((workCross$model.output[[1]])$model, c('glm', 'lm'))
  expect_is(workCross$report[[1]], 'RasterLayer')  

})




test_that('chains work.', {
  chain1 <- workflow(occurrence = Chain(UKAnophelesPlumbeus,UKAnophelesPlumbeus),
                 covariate = UKAir,
                 process = OneHundredBackground,
                 model = LogisticRegression,
                 output = SameTimePlaceMap)

  chain2 <- workflow(occurrence = UKAnophelesPlumbeus,
                 covariate = Chain(UKAir,UKAir),
                 process = OneHundredBackground,
                 model = LogisticRegression,
                 output = SameTimePlaceMap)


  chain4 <- workflow(occurrence = UKAnophelesPlumbeus,
                 covariate = UKAir,
                 process = OneHundredBackground,
                 model = LogisticRegression,
                 output = Chain(SameTimePlaceMap, SameTimePlaceMap))

  expect_true(exists('chain1'))
  expect_equal(dim(chain1$occurrence.output[[1]]), c(376, 5))
  expect_is(chain1$covariate.output[[1]], 'RasterLayer')
  expect_equal(dim(chain1$covariate.output[[1]]), c(9,9,1))
  expect_equal(names(chain1$process.output[[1]]$df), 
    c('value', 'type', 'fold', 'longitude', 'latitude', 'layer'))
  expect_equal(dim(chain1$process.output[[1]]$df),  c(457, 6))
  expect_is((chain1$model.output[[1]])$model, c('glm', 'lm'))
  expect_is(chain1$report[[1]], 'RasterLayer')  

  expect_true(exists('chain2'))
  expect_equal(dim(chain2$occurrence.output[[1]]), c(188, 5))
  expect_is(chain2$covariate.output[[1]], 'RasterStack')
  expect_equal(dim(chain2$covariate.output[[1]]), c(9,9,2))
  expect_equal(names(chain2$process.output[[1]]$df), 
    c('value', 'type', 'fold', 'longitude', 'latitude', 'layer.1', 'layer.2'))
  expect_equal(dim(chain2$process.output[[1]]$df),  c(269, 7))
  expect_is((chain2$model.output[[1]])$model, c('glm', 'lm'))
  expect_is(chain2$report[[1]], 'RasterLayer')  

  expect_true(exists('chain4'))
  expect_equal(dim(chain4$occurrence.output[[1]]), c(188, 5))
  expect_is(chain4$covariate.output[[1]], 'RasterLayer')
  expect_equal(dim(chain4$covariate.output[[1]]), c(9,9,1))
  expect_equal(names(chain4$process.output[[1]]$df), 
    c('value', 'type', 'fold', 'longitude', 'latitude', 'layer'))
  expect_equal(dim(chain4$process.output[[1]]$df),  c(269, 6))
  expect_is((chain4$model.output[[1]])$model, c('glm', 'lm'))
  expect_is(chain4$report[[1]], 'list')  


})

test_that('Local path module works.',{

  # This test will only work on unix OS.
  write(paste('#test file for zoon package\n',
              'TestModule <- function(){',
              'return(AplumbeusOcc)}'), 
        file = '~/PassingTestModule.R')

  
  
  fail1 <- workflow(occurrence = '~/PassingTestModule.R',
                    covariate = UKAir,
                    process = OneHundredBackground,
                    model = LogisticRegression,
                    output = SameTimePlaceMap)

  file.remove('~/PassingTestModule.R')
})


test_that('workflows that fail part way through, save workflow.', {

  # Only works on unix. Not sure how to fix because substitute problems.
  write(paste('#test file for zoon package\n',
              'TestModule <- function(){',
              'stop("Forced error to test error handling")}'), 
        file = '~/FailingTestModule.R')

  
  
  fail1 <- workflow(occurrence = '~/FailingTestModule.R',
                    covariate = UKAir,
                    process = OneHundredBackground,
                    model = LogisticRegression,
                    output = SameTimePlaceMap)

  expect_true(exists('tmpZoonWorkflow'))
  expect_true(all(sapply(tmpZoonWorkflow, is.null) == c(rep(TRUE, 5), FALSE)))


  rm(tmpZoonWorkflow)
  fail1 <- workflow(occurrence = UKAnophelesPlumbeus,
                    covariate = '~/FailingTestModule.R',
                    process = OneHundredBackground,
                    model = LogisticRegression,
                    output = SameTimePlaceMap)

  expect_true(exists('tmpZoonWorkflow'))
  expect_true(all(sapply(tmpZoonWorkflow, is.null) == c(FALSE, rep(TRUE, 4), FALSE)))



  rm(tmpZoonWorkflow)
  fail1 <- workflow(occurrence = UKAnophelesPlumbeus,
                    covariate = UKAir,
                    process = '~/FailingTestModule.R',
                    model = LogisticRegression,
                    output = SameTimePlaceMap)

  expect_true(exists('tmpZoonWorkflow'))
  expect_true(all(sapply(tmpZoonWorkflow, is.null) == c(FALSE, FALSE, rep(TRUE, 3), FALSE)))



  rm(tmpZoonWorkflow)
  fail1 <- workflow(occurrence = UKAnophelesPlumbeus,
                    covariate = UKAir,
                    process = OneHundredBackground,
                    model = '~/FailingTestModule.R',
                    output = SameTimePlaceMap)

  expect_true(exists('tmpZoonWorkflow'))
  expect_true(all(sapply(tmpZoonWorkflow, is.null) == c(rep(FALSE, 3), rep(TRUE, 2), FALSE)))



  rm(tmpZoonWorkflow)
  fail1 <- workflow(occurrence = UKAnophelesPlumbeus,
                    covariate = UKAir,
                    process = OneHundredBackground,
                    model = LogisticRegression,
                    output = '~/FailingTestModule.R')

  expect_true(exists('tmpZoonWorkflow'))
  expect_true(all(sapply(tmpZoonWorkflow, is.null) == c(rep(FALSE, 4), TRUE, FALSE)))

  file.remove('~/FailingTestModule.R')

})


