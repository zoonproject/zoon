context('Changing workflows')

# Original run
set.seed(1)
w1 <- workflow(UKAnophelesPlumbeus,
               UKAir, 
               OneHundredBackground,
               LogisticRegression,
               SameTimePlaceMap)

test_that('ChangeWorkflow errors', {
  
  expect_error(ChangeWorkflow(w1),
               'At least one module type must be changed')
  
  expect_error(ChangeWorkflow(w1,
                              occurrence = list(UKAnophelesPlumbeus, UKAnophelesPlumbeus),
                              covariate = list(UKAir, UKAir)),
               'Only one module type can be a list of multiple modules')
  
})

test_that('Basic ChangeWorkflow works', {

  # Change model
  set.seed(1)
  w2 <- workflow(UKAnophelesPlumbeus,
                 UKAir, 
                 OneHundredBackground,
                 RandomForest,
                 SameTimePlaceMap)

  set.seed(1)
  w3 <- ChangeWorkflow(w2, model = LogisticRegression)

  expect_true(all.equal(w1, w3))
  expect_true(identical(w2$call,
    "workflow(occurrence = UKAnophelesPlumbeus, covariate = UKAir, process = OneHundredBackground, model = RandomForest, output = SameTimePlaceMap, forceReproducible = FALSE)"))

  
  # Change Process, model and output
  set.seed(1)
  w4 <- workflow(UKAnophelesPlumbeus,
                 UKAir, 
                 BackgroundAndCrossvalid(k=2),
                 RandomForest,
                 PerformanceMeasures)

  set.seed(1)
  w5 <- ChangeWorkflow(w4,
                       process = OneHundredBackground, 
                       model = LogisticRegression, 
                       output = SameTimePlaceMap)
  expect_true(all.equal(w1, w5))
  
  
  # Change occurrence and covariate
  set.seed(1)
  sink(file=ifelse(Sys.info()["sysname"] == "Windows",
                   "NUL",
                   "/dev/null"))
  w6 <- workflow(AnophelesPlumbeus,
                 UKBioclim, 
                 OneHundredBackground,
                 LogisticRegression,
                 SameTimePlaceMap,
                 forceReproducible = TRUE)
  sink()
  
  set.seed(1)
  sink(file=ifelse(Sys.info()["sysname"] == "Windows",
                   "NUL",
                   "/dev/null"))           
  w7 <- ChangeWorkflow(w6,
                       occurrence = UKAnophelesPlumbeus,
                       covariate = UKAir,
                       forceReproducible = FALSE)
  sink()
  
  expect_true(all.equal(w1, w7))
  
  # Only change output
  # Change Process, model and output
  set.seed(1)
  w7a <- workflow(UKAnophelesPlumbeus,
                  UKAir, 
                  OneHundredBackground,
                  LogisticRegression,
                  PrintMap)
  set.seed(1)
  w7b <- ChangeWorkflow(w7a,
                        output = SameTimePlaceMap)
  expect_true(all.equal(w1, w7b))
  
})

test_that('ChangeWorkflow - Chains', {
  
  # Replace with a occurrence with chain
  set.seed(1)
  w8 <- workflow(Chain(UKAnophelesPlumbeus, UKAnophelesPlumbeus),
                 UKAir, 
                 OneHundredBackground,
                 LogisticRegression,
                 SameTimePlaceMap)
  
  set.seed(1)
  w9 <- ChangeWorkflow(w1,
                       occurrence = Chain(UKAnophelesPlumbeus, UKAnophelesPlumbeus)
                       )

  expect_true(all.equal(w9, w8))
  
  # Replace with a covariate chain
  set.seed(1)
  w10 <- workflow(UKAnophelesPlumbeus,
                  Chain(UKAir, UKAir), 
                  OneHundredBackground,
                  LogisticRegression,
                  SameTimePlaceMap)
  
  set.seed(1)
  w11 <- ChangeWorkflow(w1,
                       covariate = Chain(UKAir, UKAir)
                       )
  
  expect_true(all.equal(w10, w11))
  
  # Replace a chain with a non-chain
  set.seed(1)
  w12 <- ChangeWorkflow(w8,
                        occurrence = UKAnophelesPlumbeus)
  
  expect_true(all.equal(w12, w1))

})

test_that('ChangeWorkflow - Lists', {
  
  # Replace with a occurrence with chain
  set.seed(1)
  w13 <- workflow(list(UKAnophelesPlumbeus, UKAnophelesPlumbeus),
                 UKAir, 
                 OneHundredBackground,
                 LogisticRegression,
                 SameTimePlaceMap)
  
  set.seed(1)
  w14 <- ChangeWorkflow(w1,
                        occurrence = list(UKAnophelesPlumbeus, UKAnophelesPlumbeus)
  )
  
  expect_true(all.equal(w13, w14))
  
  # Replace with a covariate chain
  set.seed(1)
  w15 <- workflow(UKAnophelesPlumbeus,
                  list(UKAir, UKAir), 
                  OneHundredBackground,
                  LogisticRegression,
                  SameTimePlaceMap)
  
  set.seed(1)
  w16 <- ChangeWorkflow(w1,
                        covariate = list(UKAir, UKAir)
  )
  
  expect_true(all.equal(w15, w16))
  
  # Replace a chain with a non-chain
  set.seed(1)
  w17 <- ChangeWorkflow(w13,
                        occurrence = UKAnophelesPlumbeus)
  
  expect_true(all.equal(w17, w1))
  
})

test_that('More complex syntax in remaining modules works', {

 # Test module with brackets + args
 set.seed(1)
 w18 <- workflow(UKAnophelesPlumbeus,
                 UKAir, 
                 BackgroundAndCrossvalid(k=2),
                 LogisticRegression,
                 SameTimePlaceMap)

 set.seed(1)
 w19 <- ChangeWorkflow(w18, occurrence = UKAnophelesPlumbeus)

 expect_true(all.equal(w18, w19))

 set.seed(1)
 w20 <- ChangeWorkflow(w1, process = BackgroundAndCrossvalid(k = 2))
 
 expect_true(all.equal(w20, w18))
 
})