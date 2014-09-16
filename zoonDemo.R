# Simplest example

library(zoon)

work1 <- workflow(occurMod = 'UKAnophelesPlumbeus',
                  covarMod = 'UKBioclim',
                  procMod  = 'OneHundredBackground',
                  modelMod = 'LogisticRegression',
                  outMod   = 'PrintMap')

str(work1, 1)








# Help

GetModuleList()

ModuleHelp('UKBioclim')













# Modules with arguments

ModuleHelp('SpOcc')

?ModuleOptions



work2 <- workflow(occurMod = ModuleOptions('SpOcc', species = 'Loxia scotica', extent=c(-10, 10, 45, 65)),
                  covarMod = 'UKBioclim',
                  procMod  = 'OneHundredBackground',
                  modelMod = 'LogisticRegression',
                  outMod   = 'PrintMap')









# Combining modules


# Do you want 1 analysis?

?Chain
work3 <- workflow(occurMod = Chain(ModuleOptions('SpOcc', species = 'Eresus kollari', 
                                     extent=c(-10, 10, 45, 65)),
                                   ModuleOptions('SpOcc', species = 'Eresus sandaliatus', 
                                      extent=c(-10, 10, 45, 65))),
                  covarMod = 'UKBioclim',
                  procMod  = 'OneHundredBackground',
                  modelMod = 'LogisticRegression',
                  outMod   = 'PrintMap')




# Or many separate analyses



work4 <- workflow(occurMod = list(ModuleOptions('SpOcc', species = 'Eresus kollari', 
                                     extent=c(-10, 10, 45, 65)),
                                   ModuleOptions('SpOcc', species = 'Eresus sandaliatus', 
                                      extent=c(-10, 10, 45, 65))),
                  covarMod = 'UKBioclim',
                  procMod  = 'OneHundredBackground',
                  modelMod = 'LogisticRegression',
                  outMod   = 'SameTimePlaceMap')

# This should be in a module, but isn't.
par(mfrow=c(1,2))
plot(work4$output.output[[1]], main='Eresus kollari')
plot(work4$output.output[[2]], main='Eresus sandaliatus')










# Crossvalidation

ModuleHelp('BackgroundAndCrossvalid')
work5 <- workflow(occurMod = 'UKAnophelesPlumbeus',
                  covarMod = 'UKBioclim',
                  procMod  = 'BackgroundAndCrossvalid',
                  modelMod = 'LogisticRegression',
                  outMod   = 'PerformanceMeasures')

work5$output.output











# A larger example

# Note: only one module type can be in a list
#       Many module types can be chained

#       Model modules can be in a list, not in a chain

work6 <- workflow(occurMod = Chain(ModuleOptions('SpOcc', species = 'Eresus kollari', 
                                     extent=c(-10, 10, 45, 65)),
                                   ModuleOptions('SpOcc', species = 'Eresus sandaliatus', 
                                      extent=c(-10, 10, 45, 65))),
 
                  covarMod = 'UKBioclim',

                  procMod  = ModuleOptions('BackgroundAndCrossvalid', k=2),

                  modelMod = list('LogisticRegression', 'RandomForest',
                                  'QuickGRaF'),

                  outMod   = Chain('SameTimePlaceMap', 'PerformanceMeasures')
                  )

str(work6, 1)


par(mfrow=c(1,3))
plot(work6$output.output[[1]][[1]], 
  main=paste('Logistic Regression: AUC = ', round(work6$output.output[[1]][[2]]$auc, 2)))
plot(work6$output.output[[2]][[1]],
  main=paste('Random forest: AUC = ', round(work6$output.output[[2]][[2]]$auc, 2)))
plot(work6$output.output[[1]][[1]],
  main=paste('Bioclim: AUC = ', round(work6$output.output[[3]][[2]]$auc, 2)))




























