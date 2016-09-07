context('summary.zoonWorkflow')

test_that('summary.zoonWorkflow tests', {
  skip_on_cran()

  set.seed(1)
  expect_is(summary(workflow(occurrence = UKAnophelesPlumbeus,
                             covariate = UKAir,
                             process = Background(n = 70),
                             model = LogisticRegression,
                             output = SameTimePlaceMap)),
            class = 'character')
  
  set.seed(1)
  expect_is(summary(workflow(occurrence = UKAnophelesPlumbeus,
                             covariate = Chain(UKAir, UKAir, UKAir, UKAir, UKAir, UKAir),
                             process = Background(n = 70),
                             model = LogisticRegression,
                             output = SameTimePlaceMap)),
            class = 'character')
  
  set.seed(1)
  expect_is(summary(workflow(occurrence = UKAnophelesPlumbeus,
                             covariate = list(UKAir, UKAir),
                             process = Background(n = 70),
                             model = LogisticRegression,
                             output = SameTimePlaceMap)),
            class = 'character')
  
  set.seed(1)
  expect_is(summary(workflow(occurrence = UKAnophelesPlumbeus,
                             covariate = UKAir,
                             process = Background(n = 70),
                             model = LogisticRegression,
                             output = SameTimePlaceMap)),
            class = 'character')
  
  set.seed(1)
  expect_is(summary(workflow(occurrence = UKAnophelesPlumbeus,
                             covariate = UKAir,
                             process = list(Background(n = 70), NoProcess),
                             model = LogisticRegression,
                             output = SameTimePlaceMap)),
            class = 'character')
  
  set.seed(1)
  expect_is(summary(workflow(occurrence = UKAnophelesPlumbeus,
                    covariate = UKAir,
                    process = Background(n = 70),
                    model = list(OptGRaF, LogisticRegression),
                    output = SameTimePlaceMap)),
            class = 'character')
  
  set.seed(1)
  expect_is(summary(workflow(occurrence = UKAnophelesPlumbeus,
                    covariate = UKAir,
                    process = BackgroundAndCrossvalid,
                    model = OptGRaF,
                    output = list(SameTimePlaceMap, 
                                  PerformanceMeasures))),
            class = 'character')
                    
})