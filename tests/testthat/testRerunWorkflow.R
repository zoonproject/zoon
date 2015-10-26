context('RerunWorkflow')

set.seed(1)
w1 <- workflow(UKAnophelesPlumbeus,
               UKAir, 
               OneHundredBackground,
               LogisticRegression,
               SameTimePlaceMap)

test_that('RerunWorkflow simple test', {

  set.seed(1)
  w2 <- RerunWorkflow(w1)
  
  expect_true(all.equal(w1, w2))
  
})


test_that('RerunWorkflow test error', {
  
  set.seed(1)
  
  expect_error(w2 <- RerunWorkflow(w1, from = 'a'), regexp = 'Error : from')
  expect_error(w2 <- RerunWorkflow(w1, from = 6), regexp = 'Error : from')
  
  # You cannot re-run a workflow with 
  
})


test_that('RerunWorkflow test with NULLs', {
  
  w3 <- w1 
  for(i in 1:5) w3[i] <- list(NULL)
  
  w4 <- w1
  for(i in 2:5) w4[i] <- list(NULL)
  
  w5 <- w1
  for(i in 3:5) w5[i] <- list(NULL)
  
  w6 <- w1
  for(i in 4:5) w6[i] <- list(NULL)
  
  w7 <- w1
  for(i in 5) w7[i] <- list(NULL)
  
  set.seed(1)
  w8 <- RerunWorkflow(w3)
  set.seed(1)
  w9 <- RerunWorkflow(w4)
  set.seed(1)
  w10 <- RerunWorkflow(w5)
  set.seed(1)
  w11 <- RerunWorkflow(w6)
  set.seed(1)
  w12 <- RerunWorkflow(w7)
  
  expect_true(all.equal(w1, w8))
  expect_true(all.equal(w1, w9))
  expect_true(all.equal(w1, w10))
  expect_true(all.equal(w1, w11))
  expect_true(all.equal(w1, w12))
  
})


test_that('RerunWorkflow test with Chains', {
  
  set.seed(1)
  w13 <- workflow(Chain(UKAnophelesPlumbeus,UKAnophelesPlumbeus),
                 UKAir, 
                 OneHundredBackground,
                 LogisticRegression,
                 SameTimePlaceMap)
  
  set.seed(1)
  w14 <- RerunWorkflow(w13)
  expect_true(all.equal(w14, w13))
  
  set.seed(1)
  w15 <- workflow(Chain(UKAnophelesPlumbeus,UKAnophelesPlumbeus),
                  Chain(UKAir, UKAir), 
                  OneHundredBackground,
                  LogisticRegression,
                  SameTimePlaceMap)
  
  set.seed(1)
  w16 <- RerunWorkflow(w15)
  expect_true(all.equal(w15, w16))
  
})

test_that('RerunWorkflow test with lists', {
  
  set.seed(1)
  w17 <- workflow(list(UKAnophelesPlumbeus,UKAnophelesPlumbeus),
                  UKAir, 
                  OneHundredBackground,
                  LogisticRegression,
                  SameTimePlaceMap)
  
  set.seed(1)
  w18 <- RerunWorkflow(w17)
  expect_true(all.equal(w18, w17))
  
  set.seed(1)
  w19 <- workflow(UKAnophelesPlumbeus,
                  list(UKAir, UKAir), 
                  OneHundredBackground,
                  LogisticRegression,
                  SameTimePlaceMap)
  
  set.seed(1)
  w20 <- RerunWorkflow(w19)
  expect_true(all.equal(w19, w20))
  
})

test_that('RerunWorkflow test quoted modules', {
  
  set.seed(1)
  w21 <- workflow(occurrence = "UKAnophelesPlumbeus",
                  covariate  = UKAir,
                  process    = OneHundredBackground,
                  model      = RandomForest,
                  output     = PrintMap)
  
  set.seed(1)
  w22 <- RerunWorkflow(z)
  
  expect_true(all.equal(w21, w22))
  
})