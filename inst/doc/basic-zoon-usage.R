## ----setup, include=FALSE------------------------------------------------
library(knitr)
opts_chunk$set(out.extra='style="display:block; margin: auto"', fig.align="center")

## ----methods, eval = FALSE-----------------------------------------------
#  library(devtools)
#  install_github('zoonproject/zoon')

## ----load----------------------------------------------------------------
library(zoon)

## ----basic, warning = FALSE----------------------------------------------
work1 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate  = UKAir,
                  process    = OneHundredBackground,
                  model      = RandomForest,
                  output     = PrintMap)

class(work1)
str(work1, 1)

## ----getmodlist, eval = FALSE--------------------------------------------
#  GetModuleList()

## ----help, eval = FALSE--------------------------------------------------
#  ModuleHelp(LogisticRegression)

## ----args, warning = FALSE-----------------------------------------------
work2 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate  = UKAir,
                  process    = BackgroundAndCrossvalid(k = 2),
                  model      = LogisticRegression,
                  output     = PerformanceMeasures)

## ----chain, warnings = FALSE---------------------------------------------
work3 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate  = UKAir,
                  process    = Chain(OneHundredBackground, Crossvalidate),
                  model      = LogisticRegression,
                  output     = PerformanceMeasures)

## ----list, warning = FALSE-----------------------------------------------
work4 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate  = UKAir,
                  process    = OneHundredBackground,
                  model      = list(LogisticRegression, RandomForest),
                  output     = SameTimePlaceMap)

str(work4, 1)

## ----largeAnalysis, cache = TRUE, warning = FALSE------------------------
work5 <- workflow(occurrence = Chain(SpOcc(species = 'Eresus kollari', 
                                       extent = c(-10, 10, 45, 65)),
                                     SpOcc(species = 'Eresus sandaliatus', 
                                       extent = c(-10, 10, 45, 65))),
 
                  covariate  = UKAir,

                  process    = BackgroundAndCrossvalid(k = 2),

                  model      = list(LogisticRegression, RandomForest),

                  output     = Chain(SameTimePlaceMap, PerformanceMeasures)
         )

str(work5, 1)


par(mfrow=c(1,2))
plot(work5$report[[1]][[1]], 
  main = paste('Logistic Regression: AUC = ', 
             round(work5$report[[1]][[2]]$auc, 2)))
plot(work5$report[[2]][[1]],
  main = paste('Random forest: AUC = ', 
             round(work5$report[[2]][[2]]$auc, 2)))

