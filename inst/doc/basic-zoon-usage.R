## ----setup, include=FALSE------------------------------------------------
library(knitr)
opts_chunk$set(out.extra='style="display:block; margin: auto"', fig.align="center")

## ----methods-------------------------------------------------------------
library(devtools)
install_github('zoonproject/zoon')

library(zoon)

# define the extent in lat and long
uk.extent <- c(xmin = -10,
              xmax = 10,
              ymin = 45,
              ymax = 65)

## ----main analysis, eval=TRUE--------------------------------------------
# run a workflow, using the logistic regression model
ans1 <- workflow(extent = uk.extent,
                 occurrence.module = 'AnophelesPlumbeus',
                 covariate.module = 'AirNCEP',
                 process.module = 'OneHundredBackground',
                 model.module = 'LogisticRegression',
                 map.module = 'SameTimePlaceMap')

# switch the model to a RandomForest
ans2 <- workflow(extent = uk.extent,
                 occurrence.module = 'AnophelesPlumbeus',
                 covariate.module = 'AirNCEP',
                 process.module = 'OneHundredBackground',
                 model.module = 'RandomForest',
                 map.module = 'SameTimePlaceMap')


## ----output, eval=TRUE---------------------------------------------------
# look at the contents of these lists
str(ans1, 1)
str(ans2, 1)

# plot the resulting maps
par(mfrow = c(1, 2))

plot(ans1$map.output,
     zlim = c(0,1),
     main = 'LR')

points(ans1$occurrence.output[, 1:2],
       pch = 16,
       cex = 0.3)

plot(ans2$map.output,
     zlim = c(0,1),
     main = 'RF')

points(ans2$occurrence.output[, 1:2],
       pch = 16,
       cex = 0.3)

## ----storedData----------------------------------------------------------
head(ans1$occurrence.output)

