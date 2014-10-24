## ----newMod, eval = FALSE------------------------------------------------
#  NewModule <- function(df){
#  
#    zoon:::GetPackage("gam")
#  
#    covs <- as.data.frame(df[, 6:ncol(df)])
#    names(covs) <- names(df)[6:ncol(df)]
#    m <- gam::gam(formula = df$value ~ .,
#           data = covs,
#           family = binomial)
#  
#  }

## ----BuildMod, eval = FALSE----------------------------------------------
#  
#  BuildModule(NewModule, type = 'model', dir = '.', title = 'GAM sdm model',
#    description = 'This is my mega cool new model. It does GAMs using the gam package.',
#    paras = NULL, author = 'Z. Oon', email = 'zoon@zoon.com')
#  

## ----newmodworkflow, eval = FALSE----------------------------------------
#  rm(NewModule)
#  LoadModule('NewModule.R')
#  work1 <- workflow(occurrence = UKAnophelesPlumbeus,
#                    covariate = UKAir,
#                    process  = OneHundredBackground,
#                    model = NewModule,
#                    output   = PrintMap)

