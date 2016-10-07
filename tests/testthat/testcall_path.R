context('call_path')

test_that('Test call_path in a large number of variations', {
  
  skip_on_cran()
  
  CP_work <- workflow(occurrence = list(UKAnophelesPlumbeus, AnophelesPlumbeus),
                      covariate  = UKAir,
                      process    = Background(n=70),
                      model      = RandomForest,
                      output     = PrintMap)
  
  expect_equal(attr(Occurrence(CP_work)[[1]], 'call_path'),
               structure(list(covariate = "UKAnophelesPlumbeus"), .Names = "occurrence"))
  expect_equal(attr(Occurrence(CP_work)[[2]], 'call_path'),
               structure(list(covariate = "AnophelesPlumbeus"), .Names = "occurrence"))
  expect_equal(attr(Covariate(CP_work), 'call_path'),
               structure(list(covariate = "UKAir"), .Names = "covariate"))
  expect_equal(attr(Process(CP_work)[[1]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "Background"),
                         .Names = c("occurrence", "covariate", "process")))
  expect_equal(attr(Process(CP_work)[[2]], 'call_path'),
               structure(list(occurrence = "AnophelesPlumbeus", covariate = "UKAir", process = "Background"),
                         .Names = c("occurrence", "covariate", "process")))
  expect_equal(attr(Model(CP_work)[[1]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "Background", model = "RandomForest"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  expect_equal(attr(Output(CP_work)[[1]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "Background", model = "RandomForest", output = "PrintMap"),
                         .Names = c("occurrence", "covariate", "process", "model", "output")))
  expect_equal(attr(Output(CP_work)[[2]], 'call_path'),
               structure(list(occurrence = "AnophelesPlumbeus", covariate = "UKAir", process = "Background", model = "RandomForest", output = "PrintMap"),
                         .Names = c("occurrence", "covariate", "process", "model", "output")))
                            
  CP_worka <- workflow(occurrence = Chain(UKAnophelesPlumbeus, AnophelesPlumbeus),
                       covariate  = UKAir,
                       process    = Background(n=70),
                       model      = RandomForest,
                       output     = PrintMap)
  
  expect_equal(attr(Occurrence(CP_worka), 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)"), .Names = "occurrence"))
  expect_equal(attr(Process(CP_worka), 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Background"),
                         .Names = c("occurrence", "covariate", "process")))
  expect_equal(attr(Model(CP_worka), 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Background", model = "RandomForest"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  expect_equal(attr(Output(CP_worka), 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Background", model = "RandomForest", output = "PrintMap"),
                         .Names = c("occurrence", "covariate", "process", "model", "output")))
  
  CP_work2 <- workflow(occurrence = UKAnophelesPlumbeus,
                       covariate  = list(NaiveRandomRaster, UKAir),
                       process    = Background(n=70),
                       model      = RandomForest,
                       output     = PrintMap)
  
  expect_equal(attr(Covariate(CP_work2)[[1]], 'call_path'),
               structure(list(covariate = "NaiveRandomRaster"), .Names = "covariate"))
  expect_equal(attr(Covariate(CP_work2)[[2]], 'call_path'),
               structure(list(covariate = "UKAir"), .Names = "covariate"))
  expect_equal(attr(Process(CP_work2)[[1]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "NaiveRandomRaster", process = "Background"),
                         .Names = c("occurrence", "covariate", "process")))
  expect_equal(attr(Process(CP_work2)[[2]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "Background"),
                         .Names = c("occurrence", "covariate", "process")))
  expect_equal(attr(Model(CP_work2)[[1]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "NaiveRandomRaster", process = "Background", model = "RandomForest"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  expect_equal(attr(Model(CP_work2)[[2]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "Background", model = "RandomForest"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  expect_equal(attr(Output(CP_work2)[[1]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "NaiveRandomRaster", process = "Background", model = "RandomForest", output = "PrintMap"),
                         .Names = c("occurrence", "covariate", "process", "model", "output")))
  expect_equal(attr(Output(CP_work2)[[2]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "Background", model = "RandomForest", output = "PrintMap"),
                         .Names = c("occurrence", "covariate", "process", "model", "output")))
  

  CP_work2a <- workflow(occurrence = UKAnophelesPlumbeus,
                        covariate  = Chain(UKAir, UKAir),
                        process    = Background(n=70),
                        model      = RandomForest,
                        output     = PrintMap)
  
  expect_equal(attr(Covariate(CP_work2a), 'call_path'),
               structure(list(covariate = "Chain(UKAir, UKAir)"), .Names = "covariate"))
  expect_equal(attr(Process(CP_work2a), 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "Chain(UKAir, UKAir)", process = "Background"),
                         .Names = c("occurrence", "covariate", "process")))
  expect_equal(attr(Model(CP_work2a), 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "Chain(UKAir, UKAir)", process = "Background", model = "RandomForest"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  expect_equal(attr(Output(CP_work2a), 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "Chain(UKAir, UKAir)", process = "Background", model = "RandomForest", output = "PrintMap"),
                         .Names = c("occurrence", "covariate", "process", "model", "output")))
  
  
  CP_work1 <- workflow(occurrence = Chain(UKAnophelesPlumbeus, AnophelesPlumbeus),
                       covariate  = UKAir,
                       process    = list(NoProcess, Background(n=70)),
                       model      = RandomForest,
                       output     = PrintMap)
  
  expect_equal(attr(Occurrence(CP_work1), 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)"), .Names = "occurrence"))
  expect_equal(attr(Process(CP_work1)[[1]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "NoProcess"),
                         .Names = c("occurrence", "covariate","process")))
  expect_equal(attr(Process(CP_work1)[[2]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Background"),
                         .Names = c("occurrence","covariate", "process")))
  expect_equal(attr(Model(CP_work1)[[1]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "NoProcess", model = "RandomForest"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  expect_equal(attr(Model(CP_work1)[[2]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Background", model = "RandomForest"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  expect_equal(attr(Output(CP_work1)[[1]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "NoProcess", model = "RandomForest", output = "PrintMap"),
                         .Names = c("occurrence", "covariate", "process", "model", "output")))
  expect_equal(attr(Output(CP_work1)[[2]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Background", model = "RandomForest", output = "PrintMap"),
                         .Names = c("occurrence", "covariate", "process", "model", "output")))
  expect_equal(attr(Output(CP_work1)[[1]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "NoProcess", model = "RandomForest", output = "PrintMap"),
                         .Names = c("occurrence", "covariate", "process", "model", "output")))
  expect_equal(attr(Output(CP_work1)[[2]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Background", model = "RandomForest", output = "PrintMap"),
                         .Names = c("occurrence", "covariate", "process", "model", "output")))
  
  

  CP_work1a <- workflow(occurrence = Chain(UKAnophelesPlumbeus, AnophelesPlumbeus),
                        covariate  = UKAir,
                        process    = Chain(NoProcess, Background(n=70)),
                        model      = RandomForest,
                        output     = PrintMap)
  
  expect_equal(attr(Occurrence(CP_work1a), 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)"), .Names = "occurrence"))
  expect_equal(attr(Process(CP_work1a), 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Chain(NoProcess, Background)"),
                         .Names = c("occurrence", "covariate","process")))
  expect_equal(attr(Model(CP_work1a), 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Chain(NoProcess, Background)", model = "RandomForest"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  expect_equal(attr(Output(CP_work1a), 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Chain(NoProcess, Background)", model = "RandomForest", output = "PrintMap"),
                         .Names = c("occurrence", "covariate", "process", "model", "output")))
  

  CP_work3 <- workflow(occurrence = Chain(UKAnophelesPlumbeus, AnophelesPlumbeus),
                       covariate  = UKAir,
                       process    = Chain(NoProcess, Background(n=70)),
                       model      = list(RandomForest, LogisticRegression),
                       output     = PrintMap)
  
  expect_equal(attr(Model(CP_work3)[[1]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Chain(NoProcess, Background)", model = "RandomForest"),
                         .Names = c("occurrence", "covariate","process","model")))
  expect_equal(attr(Model(CP_work3)[[2]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Chain(NoProcess, Background)", model = "LogisticRegression"),
                         .Names = c("occurrence", "covariate","process","model")))
  expect_equal(attr(Output(CP_work3)[[1]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Chain(NoProcess, Background)", model = "RandomForest", output = "PrintMap"),
                         .Names = c("occurrence", "covariate", "process", "model", "output")))
  expect_equal(attr(Output(CP_work3)[[2]], 'call_path'),
               structure(list(occurrence = "Chain(UKAnophelesPlumbeus, AnophelesPlumbeus)", covariate = "UKAir", process = "Chain(NoProcess, Background)", model = "LogisticRegression", output = "PrintMap"),
                         .Names = c("occurrence", "covariate", "process", "model", "output")))
  

  CP_work3a <- workflow(occurrence = UKAnophelesPlumbeus,
                        covariate  = UKAir,
                        process    = Background(n=70),
                        model      = LogisticRegression,
                        output     = PrintMap)
  
  expect_equal(attr(Model(CP_work3a), 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "Background", model = "LogisticRegression"),
                         .Names = c("occurrence", "covariate", "process", "model")))
  expect_equal(attr(Output(CP_work3a), 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "Background", model = "LogisticRegression", output = "PrintMap"),
                         .Names = c("occurrence", "covariate", "process", "model", "output")))
  
  CP_work4 <- workflow(occurrence = UKAnophelesPlumbeus,
                       covariate  = UKAir,
                       process    = Background(n=70),
                       model      = LogisticRegression,
                       output     = list(SurfaceMap, PerformanceMeasures))
  
  expect_null(attr(Output(CP_work4)[[1]], 'call_path'))
  expect_equal(attr(Output(CP_work4)[[2]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "Background", model = "LogisticRegression", output = "PerformanceMeasures"),
                         .Names = c("occurrence","covariate", "process", "model", "output")))
  
  CP_work4b <- workflow(occurrence = UKAnophelesPlumbeus,
                       covariate  = UKAir,
                       process    = Background(n=70),
                       model      = LogisticRegression,
                       output     = Chain(SurfaceMap, PerformanceMeasures))
  expect_null(attr(Output(CP_work4b)[[1]], 'call_path'))
  expect_equal(attr(Output(CP_work4b)[[2]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "Background", model = "LogisticRegression", output = "PerformanceMeasures"),
                         .Names = c("occurrence","covariate", "process", "model", "output")))
  
  CP_work4c <- workflow(occurrence = UKAnophelesPlumbeus,
                        covariate  = UKAir,
                        process    = Background(n=70),
                        model      = list(LogisticRegression, RandomForest),
                        output     = Chain(PrintMap, PerformanceMeasures))
  expect_equal(attr(Output(CP_work4c)[[1]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "Background",
                              model = "LogisticRegression",
                              output = "PrintMap"),
                         .Names = c("occurrence","covariate", "process", "model", "output")))
  expect_equal(attr(Output(CP_work4c)[[2]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "Background",
                              model = "LogisticRegression",
                              output = "PerformanceMeasures"),
                         .Names = c("occurrence","covariate", "process", "model", "output")))
  expect_equal(attr(Output(CP_work4c)[[3]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "Background",
                              model = "RandomForest",
                              output = "PrintMap"),
                         .Names = c("occurrence","covariate", "process", "model", "output")))
  expect_equal(attr(Output(CP_work4c)[[4]], 'call_path'),
               structure(list(occurrence = "UKAnophelesPlumbeus", covariate = "UKAir", process = "Background",
                              model = "RandomForest",
                              output = "PerformanceMeasures"),
                         .Names = c("occurrence","covariate", "process", "model", "output")))
})

