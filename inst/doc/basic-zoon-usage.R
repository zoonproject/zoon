## ----setup, include=FALSE------------------------------------------------
library(knitr)
opts_chunk$set(out.extra='style="display:block; margin: auto"', fig.align="center")

## ----methods-------------------------------------------------------------
#library(devtools)
#install_github('zoonproject/zoon')

library(zoon)

## ----main analysis, eval=TRUE, cache=FALSE-------------------------------
# run a workflow, using the logistic regression model
ans1 <- workflow(occurrence.module = 'UKAnophelesPlumbeus',
                 covariate.module = 'UKAir',
                 process.module = 'OneHundredBackground',
                 model.module = 'LogisticRegression',
                 output.module = 'SameTimePlaceMap')

# switch the model to a RandomForest
ans2 <- workflow(occurrence.module = 'UKAnophelesPlumbeus',
                 covariate.module = 'UKAir',
                 process.module = 'OneHundredBackground',
                 model.module = 'RandomForest',
                 output.module = 'SameTimePlaceMap')

#ans2 <- workflow(occurrence.module = ModuleOptions('SpOcc', species = 'Anopheles plumbeus', extent = c(-10, 10, 45, 65)),
#                 covariate.module = 'UKAir',
#                 process.module = 'OneHundredBackground',
#                 model.module = 'RandomForest',
#                 output.module = 'SameTimePlaceMap')


## ----output, eval=TRUE, cache=TRUE---------------------------------------
# look at the contents of these lists
str(ans1, 1)
str(ans2, 1)

# plot the resulting maps
par(mfrow = c(1, 2))

plot(ans1$output,
     zlim = c(0,1),
     main = 'LR')

points(ans1$occurrence.output[, 1:2],
       pch = 16,
       cex = 0.3)

plot(ans2$output,
     zlim = c(0,1),
     main = 'RF')

points(ans2$occurrence.output[, 1:2],
       pch = 16,
       cex = 0.3)

## ----storedData----------------------------------------------------------
head(ans1$occurrence.output)

