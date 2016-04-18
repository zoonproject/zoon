context('setting call_path')

test_that('Test call_path in a large number of variations', {
  
  skip_on_cran()
  
  CP_work <- workflow(occurrence = list(UKAnophelesPlumbeus, AnophelesPlumbeus),
                      covariate  = UKAir,
                      process    = OneHundredBackground,
                      model      = RandomForest,
                      output     = PrintMap)
  
  expect_equal(attr(Occurrence(CP_work)[[1]], 'call_path'),
               structure(list(covariate = "UKAnophelesPlumbeus"), .Names = "occurrence"))
  expect_equal(attr(Occurrence(CP_work)[[2]], 'call_path'),
               structure(list(covariate = "AnophelesPlumbeus"), .Names = "occurrence"))
  expect_equal(attr(Covariate(CP_work), 'call_path'),
               structure(list(covariate = "UKAir"), .Names = "covariate"))
  expect_equal(attr(Process(CP_work)[[1]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "OneHundredBackground"),
                         .Names = c("occurrence", "covariate", "process")))
  expect_equal(attr(Process(CP_work)[[2]], 'call_path'),
               structure(list(occurrence = "AnophelesPlumbeus", covariate = "UKAir", process = "OneHundredBackground"),
                         .Names = c("occurrence", "covariate", "process")))
  expect_equal(attr(Model(CP_work)[[1]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "OneHundredBackground", model = "RandomForest"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  # add output check
  
  CP_worka <- workflow(occurrence = Chain(UKAnophelesPlumbeus, AnophelesPlumbeus),
                       covariate  = UKAir,
                       process    = OneHundredBackground,
                       model      = RandomForest,
                       output     = PrintMap)
  
  expect_equal(attr(Occurrence(CP_worka), 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)"), .Names = "occurrence"))
  expect_equal(attr(Process(CP_worka), 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "OneHundredBackground"),
                         .Names = c("occurrence", "covariate", "process")))
  expect_equal(attr(Model(CP_worka), 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "OneHundredBackground", model = "RandomForest"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  # add output check
  
  CP_work2 <- workflow(occurrence = UKAnophelesPlumbeus,
                       covariate  = list(UKBioclim, UKAir),
                       process    = OneHundredBackground,
                       model      = RandomForest,
                       output     = PrintMap)
  
  expect_equal(attr(Covariate(CP_work2)[[1]], 'call_path'),
               structure(list(covariate = "UKBioclim"), .Names = "covariate"))
  expect_equal(attr(Covariate(CP_work2)[[2]], 'call_path'),
               structure(list(covariate = "UKAir"), .Names = "covariate"))
  expect_equal(attr(Process(CP_work2)[[1]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKBioclim", process = "OneHundredBackground"),
                         .Names = c("occurrence", "covariate", "process")))
  expect_equal(attr(Process(CP_work2)[[2]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "OneHundredBackground"),
                         .Names = c("occurrence", "covariate", "process")))
  expect_equal(attr(Model(CP_work2)[[1]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKBioclim", process = "OneHundredBackground", model = "RandomForest"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  expect_equal(attr(Model(CP_work2)[[2]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "OneHundredBackground", model = "RandomForest"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  
  # add output check
  
  CP_work2a <- workflow(occurrence = UKAnophelesPlumbeus,
                        covariate  = Chain(UKAir, UKAir),
                        process    = OneHundredBackground,
                        model      = RandomForest,
                        output     = PrintMap)
  
  expect_equal(attr(Covariate(CP_work2a), 'call_path'),
               structure(list(covariate = "Chain(UKAir, UKAir)"), .Names = "covariate"))
  expect_equal(attr(Process(CP_work2a), 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "Chain(UKAir, UKAir)", process = "OneHundredBackground"),
                         .Names = c("occurrence", "covariate", "process")))
  expect_equal(attr(Model(CP_work2a), 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "Chain(UKAir, UKAir)", process = "OneHundredBackground", model = "RandomForest"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  # add output check
  
  CP_work1 <- workflow(occurrence = Chain(UKAnophelesPlumbeus, AnophelesPlumbeus),
                       covariate  = UKAir,
                       process    = list(NoProcess, OneHundredBackground),
                       model      = RandomForest,
                       output     = PrintMap)
  
  expect_equal(attr(Occurrence(CP_work1), 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)"), .Names = "occurrence"))
  expect_equal(attr(Process(CP_work1)[[1]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "NoProcess"),
                         .Names = c("occurrence", "covariate","process")))
  expect_equal(attr(Process(CP_work1)[[2]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "OneHundredBackground"),
                         .Names = c("occurrence","covariate", "process")))
  expect_equal(attr(Model(CP_work1)[[1]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "NoProcess", model = "RandomForest"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  expect_equal(attr(Model(CP_work1)[[2]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "OneHundredBackground", model = "RandomForest"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  expect_equal(attr(Output(CP_work1)[[1]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "NoProcess", model = "RandomForest", output = "PrintMap"),
                         .Names = c("occurrence", "covariate", "process", "model", "output")))
  expect_equal(attr(Output(CP_work1)[[2]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "OneHundredBackground", model = "RandomForest", output = "PrintMap"),
                         .Names = c("occurrence", "covariate", "process", "model", "output")))
  
  
  # add output check
  
  CP_work1a <- workflow(occurrence = Chain(UKAnophelesPlumbeus, AnophelesPlumbeus),
                        covariate  = UKAir,
                        process    = Chain(NoProcess, OneHundredBackground),
                        model      = RandomForest,
                        output     = PrintMap)
  
  expect_equal(attr(Occurrence(CP_work1a), 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)"), .Names = "occurrence"))
  expect_equal(attr(Process(CP_work1a), 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Chain(NoProcess, OneHundredBackground)"),
                         .Names = c("occurrence", "covariate","process")))
  expect_equal(attr(Model(CP_work1a), 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Chain(NoProcess, OneHundredBackground)", model = "RandomForest"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  # add output check
  
  CP_work3 <- workflow(occurrence = Chain(UKAnophelesPlumbeus, AnophelesPlumbeus),
                       covariate  = UKAir,
                       process    = Chain(NoProcess, OneHundredBackground),
                       model      = list(RandomForest, LogisticRegression),
                       output     = PrintMap)
  
  expect_equal(attr(Model(CP_work3)[[1]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Chain(NoProcess, OneHundredBackground)", model = "RandomForest"),
                         .Names = c("occurrence", "covariate","process","model")))
  expect_equal(attr(Model(CP_work3)[[2]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Chain(NoProcess, OneHundredBackground)", model = "LogisticRegression"),
                         .Names = c("occurrence", "covariate","process","model")))
  
  # add output check
  
  CP_work3a <- workflow(occurrence = UKAnophelesPlumbeus,
                        covariate  = UKAir,
                        process    = OneHundredBackground,
                        model      = LogisticRegression,
                        output     = PrintMap)
  
  expect_equal(attr(Model(CP_work3a), 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "OneHundredBackground", model = "LogisticRegression"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  # add output check
  
  CP_work4 <- workflow(occurrence = UKAnophelesPlumbeus,
                       covariate  = UKAir,
                       process    = OneHundredBackground,
                       model      = LogisticRegression,
                       output     = list(SurfaceMap, PerformanceMeasures))
  
  expect_null(attr(Output(CP_work4)[[1]], 'call_path'))
  expect_equal(attr(Output(CP_work4)[[2]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "OneHundredBackground", model = "LogisticRegression", output = "PerformanceMeasures"),
                         .Names = c("occurrence","covariate", "process", "model", "output")))
  
  #   work4a <- workflow(occurrence = UKAnophelesPlumbeus,
  #                      covariate  = UKAir,
  #                      process    = OneHundredBackground,
  #                      model      = LogisticRegression,
  #                      output     = Chain(InteractiveMap, PerformanceMeasures))
  
})

