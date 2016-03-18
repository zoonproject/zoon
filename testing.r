##  Build and reload package before execution

##  The branch 'observationCovariates' contains the working demo of the code changes
##  Use the branch master is an up-to-date fork of Zoon

GetModuleList()

#################################
## Testing different workflows ##
#################################

## Load modified versions of modules which work with the updates
source('Modified Modules/OneHundredBackground.R')
source('Modified Modules/LogisticRegression.R')
source('Modified Modules/PerformanceMeasures.R')
source('Modified Modules/Crossvalidate.R')
source('Modified Modules/UKBioclim.R')
## UKBioclim has not been modified, but saving as a local 
## function in order to test the LocalRaster module
source('Modified Modules/LocalRaster.R')
source('Modified Modules/PrintMap.R')

## Logistic regression
test1 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKBioclim,
                  process = OneHundredBackground(seed = 1),
                  model = LogisticRegression,
                  output = PerformanceMeasures)


## Cross Validation + Chains
test2 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKBioclim,
                  process = Chain(OneHundredBackground(seed = 1), Crossvalidate),
                  model = LogisticRegression,
                  output = PerformanceMeasures)

## Local Raster module
localRaster <- UKBioclim()
test3 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = LocalRaster('localRaster'),
                  process = Chain(OneHundredBackground(seed = 1)),
                  model = LogisticRegression,
                  output = PerformanceMeasures)

## Print map
test3 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKBioclim,
                  process = OneHundredBackground(seed = 1),
                  model = LogisticRegression,
                  output = PrintMap)


