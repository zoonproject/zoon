context('plot.zoonwWorkflow')

directory <- tempdir()

test_that('plot.zoonWorkflow works', {

  # Create a simple workflow to test on
  work1 <- workflow(occurrence = UKAnophelesPlumbeus,
                    covariate = UKAir,
                    process = OneHundredBackground,
                    model = LogisticRegression,
                    output = SameTimePlaceMap)
  png(filename = file.path(directory, 'tempzoonWorkflow1.png'))
  expect_null(zoon:::plot.zoonWorkflow(work1))
  dev.off()
  expect_true(file.exists(file.path(directory, 'tempzoonWorkflow1.png')))
  unlink(x = file.path(directory, 'tempzoonWorkflow1.png'))
  
  # Create a simple workflow to test on
  work2 <- workflow(occurrence = list(UKAnophelesPlumbeus, 
                                      SpOcc(species = 'Anopheles plumbeus', 
                                            extent = c(-10, 10, 45, 65))),
                    covariate = UKAir,
                    process = OneHundredBackground,
                    model = LogisticRegression,
                    output = SameTimePlaceMap)
  png(filename = file.path(directory, 'tempzoonWorkflow2.png'))
  expect_null(zoon:::plot.zoonWorkflow(work2))
  dev.off()
  expect_true(file.exists(file.path(directory, 'tempzoonWorkflow2.png')))
  unlink(x = file.path(directory, 'tempzoonWorkflow2.png'))
  
  # chain
  work3 <- workflow(occurrence = Chain(UKAnophelesPlumbeus, 
                                      SpOcc(species = 'Anopheles plumbeus', 
                                            extent = c(-10, 10, 45, 65))),
                    covariate = UKAir,
                    process = OneHundredBackground,
                    model = LogisticRegression,
                    output = SameTimePlaceMap)
  png(filename = file.path(directory, 'tempzoonWorkflow3.png'))
  expect_null(zoon:::plot.zoonWorkflow(work3))
  dev.off()
  expect_true(file.exists(file.path(directory, 'tempzoonWorkflow3.png')))
  unlink(x = file.path(directory, 'tempzoonWorkflow3.png'))
  
  #missing module
  myMissing <- function(.data){
      
      occurrence <- .data$df
      ras <- .data$ras
      
      noccurrence <- nrow(occurrence)
      
      df <- occurrence
      names(df)[6:ncol(df)] <- names(ras)
      
      return(list(df=df, ras=ras))
      
  }
  
  BuildModule(object = myMissing,
             type = 'process',
             title = 'missing',
             description = 'test',
             author = 'tom',
             email = 'tom@tom.com',
             dir = directory)
  
  rm(myMissing)
  
  LoadModule(module = file.path(directory, 'myMissing.R'))
  
  work4 <- workflow(occurrence = UKAnophelesPlumbeus, 
                    covariate = UKAir,
                    process = myMissing,
                    model = LogisticRegression,
                    output = PrintMap)
  png(filename = file.path(directory, 'tempzoonWorkflow4.png'))
  expect_null(zoon:::plot.zoonWorkflow(work4))
  dev.off()
  expect_true(file.exists(file.path(directory, 'tempzoonWorkflow4.png')))
  unlink(x = file.path(directory, 'tempzoonWorkflow4.png'))
  unlink(x = file.path(directory, 'myMissing.R'))
  
})