

# Simplest example

library(zoon)

work1 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate  = UKBioclim,
                  process    = OneHundredBackground,
                  model      = LogisticRegression,
                  output     = PrintMap)




# Help

GetModuleList()

ModuleHelp(UKBioclim)
















# Modules with arguments

ModuleHelp(SpOcc)



work2 <- workflow(occurrence = SpOcc(species = 'Loxia scotica', 
                                     extent=c(-10, 10, 45, 65)),
                  covariate  = UKBioclim,
                  process    = OneHundredBackground,
                  model      = LogisticRegression,
                  output     = PrintMap)












# Combining modules


# Do you want 1 analysis?

?Chain

work3 <- workflow(occurrence = Chain(SpOcc(species = 'Eresus kollari', 
                                           extent=c(-10, 10, 45, 65)),
                                     SpOcc(species = 'Eresus sandaliatus', 
                                           extent=c(-10, 10, 45, 65))),
                  covariate  = UKBioclim,
                  process    = OneHundredBackground,
                  model      = LogisticRegression,
                  output     = PrintMap)




# Or many separate analyses



work4 <- workflow(occurrence = list(SpOcc(species = 'Eresus kollari', 
                                          extent=c(-10, 10, 45, 65)),
                                    SpOcc(species = 'Eresus sandaliatus', 
                                          extent=c(-10, 10, 45, 65))),
                  covariate  = UKBioclim,
                  process    = OneHundredBackground,
                  model      = LogisticRegression,
                  output     = SameTimePlaceMap)

# This should be in a module, but isnt.
par(mfrow=c(1,2))
plot(work4$report[[1]], main = 'Eresus kollari')
plot(work4$report[[2]], main = 'Eresus sandaliatus')










# Crossvalidation

ModuleHelp(BackgroundAndCrossvalid)
work5 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate  = UKBioclim,
                  process    = BackgroundAndCrossvalid,
                  model      = LogisticRegression,
                  output     = PerformanceMeasures)

work5$report











# A larger example

# Note: only one module type can be in a list
#       Many module types can be chained

#       Model modules can be in a list, not in a chain

work6 <- workflow(occurrence = Chain(SpOcc(species = 'Eresus kollari', 
                                           extent=c(-10, 10, 45, 65)),
                                     SpOcc(species = 'Eresus sandaliatus', 
                                           extent=c(-10, 10, 45, 65))),
                  
                  covariate = UKBioclim,
                  
                  process  = BackgroundAndCrossvalid(k=2),
                  
                  model = list(LogisticRegression, RandomForest),
                  
                  output   = Chain(SameTimePlaceMap, PerformanceMeasures)
)

str(work6, 1)


par(mfrow=c(1,2))
plot(work6$report[[1]][[1]], 
     main=paste('Logistic Regression: AUC = ', 
                round(work6$report[[1]][[2]]$auc, 2)))
plot(work6$report[[2]][[1]],
     main=paste('Random forest: AUC = ', 
                round(work6$report[[2]][[2]]$auc, 2)))






















# Building modules
# A model module
# Input is a dataframe with columns value, type, fold, longitude, latitude then 6:ncol(df) covar columns
# Can have other arguments
NewModule <- function(df){
  
  zoon:::GetPackage("gam")
  
  covs <- as.data.frame(df[, 6:ncol(df)])
  names(covs) <- names(df)[6:ncol(df)]
  m <- gam::gam(formula = df$value ~ .,
                data = covs,
                family = binomial)
  
  # Output a model object. The object class must have a predict method available.
  # If it doesnt, define one here (see BiomodModel at link below for example)
  # https://github.com/zoonproject/modules/blob/master/R/BiomodModel.R
  return (m)
}



BuildModule(NewModule, type = "Model", dir = ".",
            description = "My cool new module")

rm(NewModule)
LoadModule('NewModule.R')
work1 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKBioclim,
                  process  = OneHundredBackground,
                  model = NewModule,
                  output   = PrintMap)




################################################################################################################

# What follows is a simple example of each module type.
# If you wish to build a module you can use these as an outline
# The names and structure of input arguments are important
# And must match these, even if some arguments arent used
# Class, strucuter and column names of return value is also constrained.

# Structure of an occurrence module

# Input can be anything
SpOcc <- function(species, extent, databases = gbif){
  
  zoon:::GetPackage(spocc)
  
  raw <- occ2df(occ(query = species, geometry = extent, from = databases, limit=10e5))
  occurrence <- raw[,c(longitude, latitude)]
  occurrence$value <- 1
  occurrence$type <- presence
  occurrence$fold <- 1
  
  # Must return a dataframe with columns longitude, latitude, value, type and fold
  # Value can be 1/0 for presence absence or numbers for abundance etc.
  # type is mostly presence, presence/absence or abundance string. Not settled yet.
  # Fold is just a column of 1s. It is currently needed (though I imagine it might be moved out of modules).
  # No other columns. Exact names please.
  return(occurrence) 
}

# Then run BuildModule to create the module file properly.
BuildModule(SpOcc, type = "Occurrence", dir = ".",
            # Be a good citizen! Describe your module well including parameters
            # These are autobuilt into documentation. 
            # Note: required arguments such as df in the model module above should not be documented.
            description = "My cool new module species occurrence module",
            paras = list(species = The species name,
                         extent = latitudinal, longitudinal extent as numeric vector,
                         databases = Character vector of databases to use from gbif, inat, ebird. Defaults to just gbif.))



# Structure of a covariate module

# Any input
LocalRaster <- function(filenames){
  
  if(is.string(filenames)){
    raster <- raster(filenames)
  } else if(is.list(filenames)) {
    rasterList <- lapply(filenames, raster)
    raster <- stack(rasterList)
  }
  
  # Must return a raster object. Layer, stack or brick.
  return(raster)
}


# Structure of process module

# Input and output are the same
# Must accept/return a list. Input list must be called data.
#   First element is a df w/ value, type, fold, longitude, latitude + covs
#   Second element is a raster layer, stack or brick.
#   Other input arguments is ok

NoProcess <- function (data) {
  
  
  occurrence <- data$df
  ras <- data$ras
  
  noccurrence <- nrow(occurrence)
  
  df <- occurrence
  names(df)[6:ncol(df)] <- names(ras)
  
  return(list(df=df, ras=ras))
}


# Structure of an output module

# Input args are a model object called model and a raster object called ras
# + Any other arguments
# I imagine this will change so it expects the occurrence data as well
SameTimePlaceMap <- function (model, ras) {
  
  # Output can be anything. Go nuts.
  
