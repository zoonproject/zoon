##  Build and reload package before execution

##  The branch 'observationCovariates' contains the working demo of the code changes
##  The branch 'observationCovariates' contains the working demo of the code changes
##  Use the branch origin/HEAD -> origion/master to see github version of code

GetModuleList()

## Load modified versions of modules which work with the updates
source('Modified Modules/OneHundredBackground.R')
source('Modified Modules/LogisticRegression.R')
source('Modified Modules/PerformanceMeasures.R')

#############
## Testing ##
#############
## Logistic regression
test1 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKBioclim,
                  process = OneHundredBackground,
                  model = LogisticRegression,
                  output = PerformanceMeasures)

source('Modified Modules/Crossvalidate.R')

## Cross Validation + Chains
test2 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKBioclim,
                  process = Chain(OneHundredBackground, Crossvalidate),
                  model = LogisticRegression,
                  output = PerformanceMeasures)


## Local Raster module
source('Modified Modules/UKBioclim.R')
## UKBioclim has not been modified, but saving as a local function in order to test the LocalRaster module
source('Modified Modules/LocalRaster.R')
localRaster <- UKBioclim()

test3 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = LocalRaster('localRaster'),
                  process = Chain(OneHundredBackground),
                  model = LogisticRegression,
                  output = PerformanceMeasures)

test3 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = localRaster,
                  process = Chain(OneHundredBackground),
                  model = LogisticRegression,
                  output = PerformanceMeasures)


