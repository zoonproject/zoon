## ----setup, include=FALSE------------------------------------------------
library(knitr)
opts_chunk$set(out.extra='style="display:block; margin: auto"', fig.align="center")

## ----packages------------------------------------------------------------
library(dismo)
library(zoon)

## ----noninteractive------------------------------------------------------
w <- workflow(UKAnophelesPlumbeus, UKAir, OneHundredBackground, LogisticRegression, PrintMap)

## ----LoadModules---------------------------------------------------------
LoadModule(UKAnophelesPlumbeus)
LoadModule(UKAir)
LoadModule(OneHundredBackground)
LoadModule(LogisticRegression)
LoadModule(PrintMap)

## ----runDataMods---------------------------------------------------------
oc <- UKAnophelesPlumbeus()
cov <- UKAir()

