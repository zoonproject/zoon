context('plot.zoonwWorkflow')

directory <- tempdir()

test_that('plot.zoonWorkflow works', {
  
  if (!capabilities('libcurl')) skip('skipping as libcurl not supported')  
  
  # Create a simple workflow to test on
  work1 <- workflow(occurrence = UKAnophelesPlumbeus,
                    covariate = UKAir,
                    process = OneHundredBackground,
                    model = LogisticRegression,
                    output = PrintMap)
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
                    output = PrintMap)
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
                    output = PrintMap)
  png(filename = file.path(directory, 'tempzoonWorkflow3.png'))
  expect_null(zoon:::plot.zoonWorkflow(work3))
  dev.off()
  expect_true(file.exists(file.path(directory, 'tempzoonWorkflow3.png')))
  unlink(x = file.path(directory, 'tempzoonWorkflow3.png'))
  
})


test_that('plot.zoonWorkflow module not on repo', {
  
  if (!capabilities('libcurl')) skip('skipping as libcurl not supported')  
  
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
             dir = directory,
             dataType = 'abundance')
  
  rm(myMissing)
  
  LoadModule(module = file.path(directory, 'myMissing.R'))
  
#   NamespaceModule <- function(){
#     return(myMissing)
#   }
#   
#   # This line accounts for the fact that testing is done
#   # in a different environment
#   assign('myMissing', NamespaceModule, env = .GlobalEnv)
  
  work4 <- workflow(occurrence = UKAnophelesPlumbeus, 
                    covariate = UKAir,
                    process = list(myMissing,
                                   OneHundredBackground,
                                   OneThousandBackground,
                                   myMissing,
                                   NoProcess),
                    model = LogisticRegression,
                    output = PrintMap)
  
  png(filename = file.path(directory, 'tempzoonWorkflow4.png'))
  expect_null(zoon:::plot.zoonWorkflow(work4))
  dev.off()
  expect_true(file.exists(file.path(directory, 'tempzoonWorkflow4.png')))
  unlink(x = file.path(directory, 'tempzoonWorkflow4.png'))
  unlink(x = file.path(directory, 'myMissing.R'))
  
  # There is some functionality for when multiple lists
  # are given... not sure if this can ever actually happen?
  # these have been commented out of the function
  
})