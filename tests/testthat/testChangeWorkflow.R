context('Changing workflows.')

test_that('Basic ChangeWorkflow works', {

  # Original run
  set.seed(1)
  w1 <- workflow(UKAnophelesPlumbeus,
                 UKAir, 
                 OneHundredBackground,
                 LogisticRegression,
                 SameTimePlaceMap)

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
                 SameTimePlaceMap)
  sink()
  
  set.seed(1)
  w7 <- ChangeWorkflow(w6,
                       occurrence = UKAnophelesPlumbeus,
                       covariate = UKAir)
  expect_true(all.equal(w1, w7))
  
  
#   # Replace with a occurrence with chain
#   set.seed(1)
#   w8 <- workflow(Chain(UKAnophelesPlumbeus, UKAnophelesPlumbeus),
#                  UKAir, 
#                  OneHundredBackground,
#                  LogisticRegression,
#                  SameTimePlaceMap)
#   
#   set.seed(1)
#   w9 <- ChangeWorkflow(w1,
#                        occurrence = Chain(UKAnophelesPlumbeus, UKAnophelesPlumbeus)
#                        )
#   
#   expect_true(all.equal(w9, w1))
#   
#   # Replace with a chain
#   set.seed(1)
#   w10 <- workflow(UKAnophelesPlumbeus,
#                   Chain(UKAir, UKAir), 
#                   OneHundredBackground,
#                   LogisticRegression,
#                   SameTimePlaceMap)
#   
#   set.seed(1)
#   w11 <- ChangeWorkflow(w1,
#                        covariate = Chain(UKAir, UKAir)
#                        )
#   
#   expect_true(all.equal(w10, w11))
  
})


# This doesn't work and I didn't have time to sort ChangeWorkflow properly
#test_that('More complex syntax in remaining modules works', {

#  # Test module with brackets + args
#  set.seed(1)
#  w6 <- workflow(UKAnophelesPlumbeus,
#                 UKAir, 
#                 BackgroundAndCrossvalid(k=2),
#                 LogisticRegression,
#                 SameTimePlaceMap)

#  set.seed(1)
#  w7 <- ChangeWorkflow(w6, occurrence = UKAnophelesPlumbeus)

#  expect_true(all.equal(w6, w7))


#  # test lists
#  set.seed(1)
#  w6 <- workflow(UKAnophelesPlumbeus,
#                 UKAir, 
#                 list(OneHundredBackground, OneThousandBackground),
#                 LogisticRegression,
#                 SameTimePlaceMap)

#  set.seed(1)
#  w7 <- ChangeWorkflow(w6, occurrence = UKAnophelesPlumbeus)

#  expect_true(all.equal(w6, w7))

#})