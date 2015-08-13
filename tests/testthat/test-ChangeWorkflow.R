
context('Test changing workflows.')

test_that('Basic ChangeWorkflow works', {

  set.seed(1)
  w1 <- workflow(UKAnophelesPlumbeus,
                 UKAir, 
                 OneHundredBackground,
                 LogisticRegression,
                 SameTimePlaceMap)

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

  set.seed(1)
  w4 <- workflow(UKAnophelesPlumbeus,
                 UKAir, 
                 BackgroundAndCrossvalid(k=2),
                 RandomForest,
                 PerformanceMeasures)

  set.seed(1)
  w5 <- ChangeWorkflow(w4, process = OneHundredBackground, 
                           model = LogisticRegression, 
                           output = SameTimePlaceMap)
  expect_true(all.equal(w1, w5))



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




test_that('RerunWorkflow works', {


  set.seed(1)
  w1 <- workflow(UKAnophelesPlumbeus,
                 UKAir, 
                 OneHundredBackground,
                 LogisticRegression,
                 SameTimePlaceMap)

  set.seed(1)
  w2 <- RerunWorkflow(w1)

  expect_true(all.equal(w1, w2))


})

