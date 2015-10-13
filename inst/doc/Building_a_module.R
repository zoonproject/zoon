## ---- eval = FALSE-------------------------------------------------------
#  NewModule <- function(.df){

## ---- eval = FALSE-------------------------------------------------------
#  # Specify the packages we need using the function
#  # GetPackage
#  zoon:::GetPackage("gam")

## ---- eval = FALSE-------------------------------------------------------
#  # Create a data.frame of covariate data
#  covs <- as.data.frame(.df[, 6:ncol(.df)])
#  names(covs) <- names(.df)[6:ncol(.df)]
#  
#  # Run our gam model
#  m <- gam::gam(formula = .df$value ~ .,
#                data = covs,
#                family = binomial)

## ---- eval = FALSE-------------------------------------------------------
#  # Create a ZoonModel object to return.
#  # this includes our model, predict method
#  # and the packages we need.
#  ZoonModel(model = m,
#            code = {
#  
#            # create empty vector of predictions
#            p <- rep(NA, nrow(newdata))
#  
#            # omit NAs in new data
#            newdata_clean <- na.omit(newdata)
#  
#            # get NA indices
#            na_idx <- attr(newdata_clean, 'na.action')
#  
#            # if there are no NAs then the index should
#            # include all rows, else it should name the
#            # rows to ignore
#            if (is.null(na_idx)){
#              na_idx <- 1:nrow(newdata)
#            } else {
#              idx <- -na_idx
#            }
#  
#            # Use the predict function in gam to predict
#            # our new values
#            p[idx] <- gam::predict.gam(model,
#                                       newdata_clean,
#                                       type = 'response')
#            return (p)
#          },
#          packages = 'gam')

## ---- eval = FALSE-------------------------------------------------------
#  NewModule <- function(.df){
#  
#    # Specify the packages we need using the function
#    # GetPackage
#    zoon:::GetPackage("gam")
#  
#    # Create a data.frame of covariate data
#    covs <- as.data.frame(.df[, 6:ncol(.df)])
#    names(covs) <- names(.df)[6:ncol(.df)]
#  
#    # Run our gam model
#    m <- gam::gam(formula = .df$value ~ .,
#           data = covs,
#           family = binomial)
#  
#    # Create a ZoonModel object to return.
#    # this includes our model, predict method
#    # and the packages we need.
#    ZoonModel(model = m,
#              code = {
#  
#              # create empty vector of predictions
#              p <- rep(NA, nrow(newdata))
#  
#              # omit NAs in new data
#              newdata_clean <- na.omit(newdata)
#  
#              # get their indices
#              na_idx <- attr(newdata_clean, 'na.action')
#  
#              # if there are no NAs then the index should
#              # include all rows, else it should name the
#              # rows to ignore
#              if (is.null(na_idx)){
#                na_idx <- 1:nrow(newdata)
#              } else {
#                idx <- -na_idx
#              }
#  
#              # Use the predict function in gam to predict
#              # our new values
#              p[idx] <- gam::predict.gam(model,
#                                         newdata_clean,
#                                         type = 'response')
#              return (p)
#            },
#            packages = 'gam')
#  
#  }

## ----BuildMod, eval = FALSE----------------------------------------------
#  BuildModule(object = NewModule,
#              type = 'model',
#              dir = '.',
#              title = 'GAM sdm model',
#              description = 'This is my mega cool new model.',
#              details = 'This module performs GAMs (Generalised Additive Models) using the \\code{gam} function from the package \\code{gam}.',
#              paras = NULL,
#              author = 'Z. Oon',
#              email = 'zoon@zoon.com')

## ----newmodworkflow, eval = FALSE----------------------------------------
#  rm(NewModule)
#  LoadModule('NewModule.R')
#  work1 <- workflow(occurrence = UKAnophelesPlumbeus,
#                    covariate = UKAir,
#                    process  = OneHundredBackground,
#                    model = NewModule,
#                    output   = PrintMap)

