
context('Test workflow function.')

test_that('simple, local data workflow works.', {
  work1 <- workflow(occurMod = 'UKAnophelesPlumbeus',
                 covarMod = 'UKAir',
                 procMod = 'OneHundredBackground',
                 modelMod = 'LogisticRegression',
                 outMod = 'SameTimePlaceMap')

  expect_true(exists('work1'))
  expect_equal(names(work1), c('occurrence.output', 'covariate.output', 'process.output', 'model.output', 'output'))
  expect_equal(dim(work1$occurrence.output), c(199, 4))
  expect_is(work1$covariate.output, 'RasterLayer')
  expect_equal(dim(work1$covariate.output), c(9,9,1))
  expect_equal(names(work1$process.output, c('value', 'type',  'lon',   'lat',   'layer'))
  expect_equal(dim(work1$process.output),  c(280, 5))
  expect_is(work1$model.output, c('glm', 'lm'))
  expect_is(work1$output, 'RasterLayer')
  

})

test_that('modules downloading data work', {

  work2 <- workflow(occurMod = ModuleOptions('SpOcc', species = 'Anopheles plumbeus', extent = c(-20, 20, 45, 65)),
                   covarMod = ModuleOptions('NCEP', variables = c('air', 'rhum'), extent = c(-20,20,45,65)),
                   procMod = 'OneHundredBackground',
                   modelMod = ModuleOptions('BiomodModel', modelType = c('GAM')),
                   outMod = 'SameTimePlaceMap')
  
  expect_true(exists('work2'))
  expect_equal(names(work2), c('occurrence.output', 'covariate.output', 'process.output', 'model.output', 'output'))
  expect_is(work2$occurrence.output, 'data.frame')
  expect_equal(names(work2$occurrence.output), c('longitude', 'latitude', 'value', 'type'))
  expect_true(all(work2$occurrence.output[,'longitude'] < 20) & all(work2$occurrence.output[,'longitude'] > -20))
  expect_true(all(work2$occurrence.output[,'latitude'] < 65) & all(work2$occurrence.output[,'latitude'] > 45))
  expect_true(all(work2$occurrence.output[,'type']=='presence'))
  expect_is(work2$covariate.output, 'RasterStack')
  expect_is(work2$model.output, 'BIOMOD.models.out')
  expect_is(work2$output, 'RasterLayer')
})

test_that('collecting modules with names and urls is equivelent.', {
  set.seed(1)
  workNames <- workflow(occurMod = 'UKAnophelesPlumbeus',
                 covarMod = 'UKAir',
                 procMod = 'OneHundredBackground',
                 modelMod = 'LogisticRegression',
                 outMod = 'SameTimePlaceMap')
  set.seed(1)
  workURLs <- workflow(occurMod = 'https://raw.githubusercontent.com/zoonproject/modules/master/R/UKAnophelesPlumbeus.R',
                 covarMod = 'https://raw.githubusercontent.com/zoonproject/modules/master/R/UKAir.R',
                 procMod = 'https://raw.githubusercontent.com/zoonproject/modules/master/R/OneHundredBackground.R',
                 modelMod = 'https://raw.githubusercontent.com/zoonproject/modules/master/R/LogisticRegression.R',
                 outMod = 'https://raw.githubusercontent.com/zoonproject/modules/master/R/SameTimePlaceMap.R')

expect_equal(workNames, workURLs)

}

