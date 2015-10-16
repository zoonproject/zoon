## ----setup, include=FALSE------------------------------------------------
library(knitr)
opts_chunk$set(out.extra='style="display:block; margin: auto"', fig.align="center")

## ----packages------------------------------------------------------------
library(dismo)
library(zoon)

## ----noninteractive------------------------------------------------------
w <- workflow(UKAnophelesPlumbeus, UKAir, OneHundredBackground, LogisticRegression, PrintMap)

## ----LoadModules---------------------------------------------------------
LoadModule('UKAnophelesPlumbeus')
LoadModule('UKAir')
LoadModule('OneHundredBackground')
LoadModule('LogisticRegression')
LoadModule('PrintMap')

## ----runDataMods---------------------------------------------------------
oc <- UKAnophelesPlumbeus()
cov <- UKAir()

## ----extract-------------------------------------------------------------
data <- zoon:::ExtractAndCombData(oc, cov)

## ----procAndModel--------------------------------------------------------
proc <- OneHundredBackground(data)

mod <- LogisticRegression(proc$df)

## ----output--------------------------------------------------------------
model <- list(model = mod, data = proc$df)

out <- PrintMap(model, cov)

## ----cross validation----------------------------------------------------
modCrossvalid <- zoon:::RunModels(proc$df, 'LogisticRegression', list(), environment())

modelCrossvalid <- list(model = modCrossvalid$model, data = proc$df)

out <- PrintMap(modelCrossvalid, cov)

