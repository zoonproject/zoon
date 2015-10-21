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

