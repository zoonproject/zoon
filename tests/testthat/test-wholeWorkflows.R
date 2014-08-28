
#context('Test workflow function.')
#
#test_that('Workflows with only one module per type', {
#  work1 <- workflow(occurMod = 'UKAnophelesPlumbeus',
#                 covarMod = 'UKAir',
#                 procMod = 'OneHundredBackground',
#                 modelMod = 'LogisticRegression',
#                 outMod = 'SameTimePlaceMap')
#
#  work2 <- workflow(occurMod = 'UKAnophelesPlumbeus',
#                 covarMod = 'UKAir',
#                 procMod = 'OneHundredBackground',
#                 modelMod = ModuleOptions('BiomodModel', modelType = c('GLM')),
#                 outMod = 'SameTimePlaceMap')
#
#  work3 <- workflow(occurMod = ModuleOptions('SpOcc', species = 'Anopheles plumbeus', extent = c(-10, 10, 45, 65)),
#                 covarMod = 'UKAir',
#                 procMod = 'OneHundredBackground',
#                 modelMod = ModuleOptions('BiomodModel', modelType = c('GAM')),
#                 outMod = 'SameTimePlaceMap')
#
#  expect_true(all(exists(
#  
#
#})
#
