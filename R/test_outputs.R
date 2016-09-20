#' @importFrom SDMTools auc confusion.matrix
#' @import randomForest
#' 
# A function for testing the outputs conform to the expected
# (within a context)
test_outputs <- function(roxy_parse, modulePath){

  # create enviromnet for zoon to find variables
  pos <- 1
  en <- as.environment(pos)

  # Appease CRAN
  # The modules in these checks appear to have no global definition
  # I get around this by assigning them all to NULL here and then loading 
  # them in. using forcereproducile = TRUE is not an option as this does
  # not allow me to use the module under test
  
  test_that(paste('Check OCCURRENCE output formats for', basename(modulePath)),{ 
    
    ## OCCURRENCE MODULES ##
    if(roxy_parse$family == 'occurrence'){
      
      Background <- BackgroundAndCrossvalid <- NULL
      NaiveRandomRaster <- Crossvalidate <- LogisticRegression <- NULL
      NoProcess <- PrintMap <- NULL
      
      ## Move these to the sections where they are needed
      LoadModule('Background'); LoadModule('BackgroundAndCrossvalid')
      LoadModule('Crossvalidate'); LoadModule('LogisticRegression')
      LoadModule('NaiveRandomRaster'); LoadModule('NoProcess')
      LoadModule('PrintMap')
      
      # Load the script
      source(modulePath) 
      
      # Run the module with defaults
      occ_return <- tryCatchModule(expr = {occ_return <- do.call(roxy_parse$name, args = list())},
                     code_chunk = paste0(roxy_parse$name, '()'),
                     fun = roxy_parse$name)
      if(inherits(occ_return, what = 'moduleError')) stop(occ_return)

      # Check the data.frame returned is as expected
      expect_is(occ_return, 'data.frame', info = 'Occurrence modules must return a data.frame')
      expect_named(occ_return, expected = c('longitude',
                                            'latitude',
                                            'value',
                                            'type',
                                            'fold'), 
                   info = "Check your occurrence data has the correct column names in the correct order. See the 'Building a module' vignette for details")
      expect_is(occ_return$longitude, c('numeric', 'integer'), info = 'longitude must be a numeric or integer')
      expect_is(occ_return$latitude, c('numeric', 'integer'), info = 'latitude must be a numeric or integer')
      expect_is(occ_return$value, c('numeric', 'integer'), info = 'value must be a numeric or integer')
      expect_is(occ_return$type, 'character', info = 'type must be a character')
      expect_is(occ_return$fold, c('numeric', 'integer'), info = 'info must be a numeric or integer')
      
      ## Check it in a few workflows
      # Assign function
      OccurrenceModule <- source(modulePath)$value
      # this line is needed to  it to the global env. where zoon looks
      assign('OccurrenceModule', OccurrenceModule, envir = en)
      
      # Get the data types
      sections <- roxy_parse[grepl('section', names(roxy_parse))]
      data_types <- trimws(strsplit(gsub('Data type: ', '', sections[grepl('Data type: ', sections)]),
                                    split = ',')[[1]])
      
      xmin <- floor(min(occ_return$longitude, na.rm = TRUE))
      xmax <- ceiling(max(occ_return$longitude, na.rm = TRUE))
      ymin <- floor(min(occ_return$latitude, na.rm = TRUE))
      ymax <- ceiling(max(occ_return$latitude, na.rm = TRUE))
      
      myExtent <- c(xmin, xmax, ymin, ymax)
      
      # We have to do this so that zoon can find it
      assign('myExtent', myExtent, envir = en)
      
      for(data_type in data_types){
        
        # Presence only
        if(data_type == "presence-only"){ 
          
          # test normal
          w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = OccurrenceModule,
                                                      covariate = NaiveRandomRaster(extent = myExtent,
                                                                                    res = 0.5,
                                                                                    seed = 123),
                                                      process = Background(n = 70),
                                                      model = LogisticRegression,
                                                      output = PrintMap,
                                                      forceReproducible = FALSE)},
                                placeholder = 'OccurrenceModule',
                                fun = roxy_parse$name)
          if(inherits(w, what = 'moduleError')) stop(w)
          
          expect_is(w, 'zoonWorkflow',
                    info = 'The occurrence module did not work in a standard workflow')
          
          # test Chain
          w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = Chain(OccurrenceModule,
                                                                         OccurrenceModule),
                                                      covariate = NaiveRandomRaster(extent = myExtent,
                                                                                    res = 0.5,
                                                                                    seed = 123),
                                                      process = Background(n = 70),
                                                      model = LogisticRegression,
                                                      output = PrintMap,
                                                      forceReproducible = FALSE)},
                                placeholder = 'OccurrenceModule',
                                fun = roxy_parse$name)
          if(inherits(w, what = 'moduleError')) stop(w)
          
          expect_is(w, 'zoonWorkflow',
                    info = 'The occurrence module did not work when chained in a workflow')
          

          # test list + crossvalidation
          w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = list(OccurrenceModule,
                                                                        OccurrenceModule),
                                                      covariate = NaiveRandomRaster(extent = myExtent,
                                                                                    res = 0.5,
                                                                                    seed = 123),
                                                      process = BackgroundAndCrossvalid,
                                                      model = LogisticRegression,
                                                      output = PerformanceMeasures,
                                                      forceReproducible = FALSE)},
                                placeholder = 'OccurrenceModule',
                                fun = roxy_parse$name)
          if(inherits(w, what = 'moduleError')) stop(w)
          
          expect_is(w, 'zoonWorkflow',
                    info = 'The occurrence module did not work when listed in a workflow with crossvalidation')
          
          
        } else if(data_type == "presence/absence"){
          
            # test normal
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = OccurrenceModule,
                                                        covariate = NaiveRandomRaster(extent = myExtent,
                                                                                      res = 0.5,
                                                                                      seed = 123),
                                                        process = NoProcess,
                                                        model = LogisticRegression,
                                                        output = PrintMap,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'OccurrenceModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
          
            expect_is(w, 'zoonWorkflow',
                      info = 'The occurrence module did not work in a standard workflow')
  
            # test Chain
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = Chain(OccurrenceModule,
                                                                           OccurrenceModule),
                                                        covariate = NaiveRandomRaster(extent = myExtent,
                                                                                      res = 0.5,
                                                                                      seed = 123),
                                                        process = NoProcess,
                                                        model = LogisticRegression,
                                                        output = PrintMap,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'OccurrenceModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
          
            expect_is(w, 'zoonWorkflow',
                      info = 'The occurrence module did not work when chained in a workflow')
  
  
  
            # test list + crossvalidation
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = list(OccurrenceModule,
                                                                          OccurrenceModule),
                                                        covariate = NaiveRandomRaster(extent = myExtent,
                                                                                      res = 0.5,
                                                                                      seed = 123),
                                                        process = Crossvalidate,
                                                        model = LogisticRegression,
                                                        output = PerformanceMeasures,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'OccurrenceModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
            
            expect_is(w, 'zoonWorkflow',
                      info = 'The occurrence module did not work when listed in a workflow with crossvalidation')

        } ## Add tests for proportion and abundance ##
      }
    }
  })
  
  test_that(paste('Check COVARIATE output formats for', basename(modulePath)),{ 
    
    ## COVARIATE MODULES ##
    if(roxy_parse$family == 'covariate'){
      
      Background <- BackgroundAndCrossvalid <- NULL
      LogisticRegression <- NaiveRandomPresence <- NULL
      PrintMap <- NULL
      
      ## Move these to the sections where they are needed
      LoadModule('Background'); LoadModule('BackgroundAndCrossvalid')
      LoadModule('LogisticRegression'); LoadModule('NaiveRandomPresence');
      LoadModule('PrintMap')
      
      # Load the script
      source(modulePath) 
      
      # Run the module with defaults
      cov_return <- tryCatchModule(expr = {cov_return <- do.call(roxy_parse$name, args = list())},
                                   code_chunk = paste0(roxy_parse$name, '()'),
                                   fun = roxy_parse$name)
      if(inherits(cov_return, what = 'moduleError')) stop(cov_return)

      # Check projection
      expect_true(all(grepl("+proj=longlat", projection(cov_return)),
                      grepl("+ellps=WGS84", projection(cov_return))),
                  info = 'Covariate module output must have WGS84 projection: proj4 string is expected to contain the elements "+proj=longlat" and "+ellps=WGS84"')
      
      # Check raster returned is as expected
      expect_is(cov_return, c('RasterLayer', 'RasterStack', 'RasterBrick'), info = 'Covariate module output must be either a RasterLayer or a RasterStack')
      
      ## Check it in a few workflows
      # Assign function
      CovariateModule <- source(modulePath)$value
      # this line is needed to  it to the global env. where zoon looks
      assign('CovariateModule', CovariateModule, envir = en)
      
      expect_is(extent(cov_return), "Extent",
                info = 'An extent could not be obtained from the object returned by the covariate module')
      
      myExtent <- as.vector(extent(cov_return))
      
      # We have to do this so that zoon can find it
      assign('myExtent', myExtent, envir = en)
      
      # Normal
      w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = NaiveRandomPresence(extent = myExtent,
                                                                                   n = 1000,
                                                                                   seed = 123),
                                                  covariate = CovariateModule,
                                                  process = Background(n = 70),
                                                  model = LogisticRegression,
                                                  output = PrintMap,
                                                  forceReproducible = FALSE)},
                            placeholder = 'CovariateModule',
                            fun = roxy_parse$name)
      if(inherits(w, what = 'moduleError')) stop(w)
      
      expect_is(w, 'zoonWorkflow',
                info = 'The covariate module did not work in a standard workflow')
      
      # Chain
      w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = NaiveRandomPresence(extent = myExtent,
                                                                                   n = 1000,
                                                                                   seed = 123),
                                                  covariate = Chain(CovariateModule, CovariateModule),
                                                  process = Background(n = 70),
                                                  model = LogisticRegression,
                                                  output = PrintMap,
                                                  forceReproducible = FALSE)},
                            placeholder = 'CovariateModule',
                            fun = roxy_parse$name)
      if(inherits(w, what = 'moduleError')) stop(w)
      
      expect_is(w, 'zoonWorkflow',
                info = 'The covariate module did not work when chained in a workflow')
      
      # List + crossvalidate
      w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = NaiveRandomPresence(extent = myExtent,
                                                                                   n = 1000,
                                                                                   seed = 123),
                                                  covariate = list(CovariateModule, CovariateModule),
                                                  process = BackgroundAndCrossvalid,
                                                  model = LogisticRegression,
                                                  output = PrintMap,
                                                  forceReproducible = FALSE)},
                            placeholder = 'CovariateModule',
                            fun = roxy_parse$name)
      if(inherits(w, what = 'moduleError')) stop(w)
      
      expect_is(w, 'zoonWorkflow',
                info = 'The covariate module did not work when listed in a workflow with crossvalidation')
      
    }
  })
  
  
  test_that(paste('Check PROCESS output formats for', basename(modulePath)),{ 
    
    ## PROCESS MODULES ##
    if(roxy_parse$family == 'process'){
      
      Background <- NaiveRandomPresenceAbsence <- NULL
      NaiveRandomRaster <- Crossvalidate <- LogisticRegression <- NULL
      NoProcess <- NULL
      PrintMap <- RandomForest <- UKAir <- UKAnophelesPlumbeus <- NULL
      
      ## Move these to the sections where they are needed
      LoadModule('Background'); LoadModule('NaiveRandomPresenceAbsence')
      LoadModule('Crossvalidate'); LoadModule('LogisticRegression')
      LoadModule('NaiveRandomRaster'); LoadModule('NoProcess')
      LoadModule('PrintMap'); LoadModule('RandomForest')
      LoadModule('UKAir'); LoadModule('UKAnophelesPlumbeus')
      
      # Load the script
      source(modulePath) 
      
      # Run the module with defaults
      sections <- roxy_parse[grepl('section', names(roxy_parse))]
      data_types <- gsub('Data type: ', '', sections[grepl('Data type: ', sections)])
      
      # assign the module
      ProcessModule <- source(modulePath)$value
      # this line is needed to  it to the global env. where zoon looks
      assign('ProcessModule', ProcessModule, envir = en)
      
      for(data_type in c("presence/absence", "presence-only")){
        
        if(grepl(data_type, data_types)){
          
          # visible binding
          .data <- NULL  
           
          # Choose file to load
          if(data_type == "presence/absence"){
            loadExpr <- substitute(load(system.file("extdata", "data_PA.rdata", package="zoon")))
          } else if(data_type == "presence-only"){
            loadExpr <- substitute(load(system.file("extdata", 'data_PO.rdata', package="zoon")))
          } 
          
          # run load expression
          eval(loadExpr)
          
          # Run the module with defaults
          pro_return <- tryCatchModule(expr = {pro_return <- do.call(roxy_parse$name, args = list(.data = .data))},
                                       code_chunk = paste(capture.output(print(loadExpr)),
                                                          '\n', paste0(roxy_parse$name, '(.data = .data)')),
                                       fun = roxy_parse$name)
          if(inherits(pro_return, what = 'moduleError')) stop(pro_return)
          
          ## Check pro_return structure
          expect_is(pro_return, 'list', info = 'The object returned from a process module must be a list')
          expect_named(pro_return, expected = c('df', 'ras'), info = 'The elements of the list returned from a process module must be named "df" and "ras"')
          
          ## Check 'df'
          # Check the data.frame returned is as expected
          expect_is(pro_return$df, 'data.frame', info = 'Occurrence modules must return a data.frame')
          expect_true(all(c('longitude','latitude','value','type','fold') %in% names(pro_return$df)), 
                      info = "Some of the required columns from the 'df' element returned by the process module, are missing ('longitude', 'latitude', 'value', 'type', 'fold')")
          expect_is(pro_return$df$longitude, c('numeric', 'integer'), info = 'longitude column, from the "df" element returned from a process module, must be a numeric or integer')
          expect_is(pro_return$df$latitude, c('numeric', 'integer'), info = 'latitude column, from the "df" element returned from a process module, must be a numeric or integer')
          expect_is(pro_return$df$value, c('numeric', 'integer'), info = 'value column, from the "df" element returned from a process module, must be a numeric or integer')
          expect_is(pro_return$df$type, c('character', 'factor'), info = 'type column, from the "df" element returned from a process module, must be a character')
          expect_is(pro_return$df$fold, c('numeric', 'integer'), info = 'info column, from the "df" element returned from a process module, must be a numeric or integer')
          expect_true(ncol(pro_return$df) >= 6, info = 'The "df" element returned from a process module is expected to contain 6 or more columns')
          
          ## Check 'ras'
          # Check projection
          expect_true(all(grepl("+proj=longlat", projection(pro_return$ras)),
                          grepl("+ellps=WGS84", projection(pro_return$ras))),
                      info = 'The "ras" element returned by a process module must have WGS84 projection: proj4 string is expected to contain the elements "+proj=longlat" and "+ellps=WGS84"')
          
          # Check raster returned is as expected
          expect_is(pro_return$ras, c('RasterLayer', 'RasterStack', 'RasterBrick'), info = 'The "ras" element returned by a process module must be either a RasterLayer or a RasterStack')
          
          ## Try it in some workflows
          if(data_type == "presence/absence"){
            
            # normal
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = NaiveRandomPresenceAbsence(n = 1000, seed = 123),
                                                        covariate = NaiveRandomRaster,
                                                        process = ProcessModule,
                                                        model = LogisticRegression,
                                                        output = PrintMap,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'ProcessModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
            
            
            expect_is(w, 'zoonWorkflow',
                      info = 'The process module did not work in a standard workflow')
            
            # Chained
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = UKAnophelesPlumbeus,
                                                        covariate = UKAir,
                                                        process = Chain(Background(n = 70),
                                                                        ProcessModule),
                                                        model = LogisticRegression,
                                                        output = PrintMap,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'ProcessModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
            
            expect_is(w, 'zoonWorkflow',
                      info = 'The process module did not work in a chain workflow')
            
            # list + crossvalidated
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = NaiveRandomPresenceAbsence(n = 1000, seed = 123),
                                                        covariate = NaiveRandomRaster,
                                                        process = list(Crossvalidate,
                                                                       ProcessModule),
                                                        model = LogisticRegression,
                                                        output = PrintMap,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'ProcessModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
            
            expect_is(w, 'zoonWorkflow',
                      info = 'The process module did not work in a list workflow')
            
            
          } else if(data_type == "presence-only"){
            
            # Norm
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = UKAnophelesPlumbeus,
                                                        covariate = UKAir,
                                                        process = ProcessModule,
                                                        model = RandomForest,
                                                        output = PrintMap,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'ProcessModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
            
            expect_is(w, 'zoonWorkflow',
                      info = 'The process module did not work in a standard workflow')
            
            # Chain
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = UKAnophelesPlumbeus,
                                                        covariate = UKAir,
                                                        process = list(NoProcess,
                                                                       ProcessModule),
                                                        model = RandomForest,
                                                        output = PrintMap,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'ProcessModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
            
            expect_is(w, 'zoonWorkflow',
                      info = 'The process module did not work in a chain workflow')
            
            # list
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = UKAnophelesPlumbeus,
                                                        covariate = UKAir,
                                                        process = list(NoProcess,
                                                                       ProcessModule),
                                                        model = RandomForest,
                                                        output = PrintMap,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'ProcessModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
            
            expect_is(w, 'zoonWorkflow',
                      info = 'The process module did not work in a list workflow')
          }
        }
      }
    }
  })
  
  test_that(paste('Check MODEL output formats for', basename(modulePath)),{ 
  
    ## MODEL MODULES ##
    if(roxy_parse$family == 'model'){
      
      Background <- NaiveRandomRaster <- Crossvalidate <- NULL
      NaiveRandomPresenceAbsence <- NoProcess <- PerformanceMeasures <- NULL
      PrintMap <- UKAir <- UKAnophelesPlumbeus <- NULL
      
      ## Move these to the sections where they are needed
      LoadModule('Background'); LoadModule('Crossvalidate')
      LoadModule('NaiveRandomRaster'); LoadModule('NaiveRandomPresenceAbsence')
      LoadModule('NoProcess'); LoadModule('PerformanceMeasures');
      LoadModule('PrintMap'); LoadModule('UKAir'); LoadModule('UKAnophelesPlumbeus')
      
      # Load the script
      source(modulePath) 
      
      # Run the module with defaults
      sections <- roxy_parse[grepl('section', names(roxy_parse))]
      data_types <- gsub('Data type: ', '', sections[grepl('Data type: ', sections)])
      
      # assign the module
      ModelModule <- source(modulePath)$value
      # this line is needed to  it to the global env. where zoon looks
      assign('ModelModule', ModelModule, envir = en)
      
      if(length(data_types) > 0){
      
        # There are not any true presence only models as yet
        for(data_type in c("presence/absence", "presence/background")){
        
        if(grepl(data_type, data_types)){
          
          # Choose file to load
          if(data_type == "presence/absence"){
            loadExpr <- substitute(load(system.file("extdata", 'data_PA.rdata', package="zoon")))
          } else if(data_type == "presence/background"){
            loadExpr <- substitute(load(system.file("extdata", 'data_PB.rdata', package="zoon")))
          } 
          
          # run load expression
          eval(loadExpr)
          
          # Run the module with defaults
          mod_return <- tryCatchModule(expr = {mod_return <- do.call(roxy_parse$name, args = list(.df = .data$df))},
                                       code_chunk = paste(capture.output(print(loadExpr)),
                                                          '\n', paste0(roxy_parse$name, '(.df = .data$df)')),
                                       fun = roxy_parse$name)
          if(inherits(mod_return, what = 'moduleError')) stop(mod_return)
          
          ## Check mod_return structure
          expect_is(mod_return, 'zoonModel', info = "The object returned from a model module must be a 'zoonModel' object. See '?ZoonModel' for more details")
          expect_named(mod_return, expected = c('model', 'code', 'packages'), info = "The elements of a 'zoonModel' object must be named 'model', 'code', and 'packages'")
          
          
          ## Check 'model'
          # There are not any requirements other than that is exists
          
          
          ## Check 'code'
          # code is a charachter and can be parsed and checked
          expect_is(mod_return$code, 'character', info = 'The "code" element returned by a model module must be a character (see ?ZoonModel for details)')
          
          newdata <- .data$df[, 6:ncol(.data$df), drop = FALSE]

          predicted_vals <- ZoonPredict(zoonModel = mod_return,
                                        newdata = newdata)
          
          expect_true(is.vector(predicted_vals), info = 'Your ZoonModel object (returned from your model module) does not return a vector when using "code" to predict (see ?ZoonModel)')
          expect_is(predicted_vals, 'numeric', info = 'The code element of your model module did not return a vector of predicted values when given your model object and data.')
          expect_equal(length(predicted_vals), nrow(newdata), info = 'Length of predicted values is not equal to the number of row in newdata')
          
          
          ## Check 'packages'
          expect_true(is.vector(mod_return$packages), info = '"packages" element of ZoonModel object returned from a model module should be a vector')
          expect_is(mod_return$packages, 'character', '"packages" element of ZoonModel object returned from a model module should be a character object of package names')
          
          # Check these against available packages
          cran_avail_packages <- unique(row.names(utils::available.packages('http://cran.rstudio.com/src/contrib')))
          inst_pck <- as.data.frame(utils::installed.packages())
          base_installed_packages <- rownames(inst_pck[inst_pck$Priority=="base" & !is.na(inst_pck$Priority), ])
          avail_packages <- c(cran_avail_packages, base_installed_packages)
          not_avail <- mod_return$packages[!mod_return$packages %in% avail_packages]
          
          expect_true(length(not_avail) == 0, info = paste('Not all packages specified in your model module are available on cran. Could not find:',
                                                           paste(not_avail, collapse = ', ')))
          
          ## Try it in some workflows
          if(data_type == "presence/absence"){
            
            # normal
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = NaiveRandomPresenceAbsence(n = 1000, seed = 123),
                                                        covariate = NaiveRandomRaster,
                                                        process = NoProcess,
                                                        model = ModelModule,
                                                        output = PrintMap,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'ModelModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
            
            expect_is(w, 'zoonWorkflow',
                      info = 'The model module did not work in a standard workflow')
            
            # Chained
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = NaiveRandomPresenceAbsence(n = 1000, seed = 123),
                                                        covariate = NaiveRandomRaster,
                                                        process = Chain(NoProcess, NoProcess),
                                                        model = ModelModule,
                                                        output = PrintMap,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'ModelModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
            
            expect_is(w, 'zoonWorkflow',
                      info = 'The model module did not work in a chain workflow')
            
            # list
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = NaiveRandomPresenceAbsence(n = 1000, seed = 123),
                                                        covariate = NaiveRandomRaster,
                                                        process = NoProcess,
                                                        model = list(ModelModule, ModelModule),
                                                        output = PrintMap,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'ModelModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
            
            expect_is(w, 'zoonWorkflow',
                      info = 'The model module did not work in a list workflow')
            
            # crossvalidate
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = NaiveRandomPresenceAbsence(n = 1000, seed = 123),
                                                        covariate = NaiveRandomRaster,
                                                        process = Crossvalidate,
                                                        model = list(ModelModule, ModelModule),
                                                        output = PerformanceMeasures,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'ModelModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
            
            expect_is(w, 'zoonWorkflow',
                      info = 'The process module did not work in a crossvalidation workflow')
            
            
          } else if(data_type == "presence/background"){
            
            # Norm
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = UKAnophelesPlumbeus,
                                                        covariate = UKAir,
                                                        process = Background(n = 70),
                                                        model = ModelModule,
                                                        output = PrintMap,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'ModelModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
            
            expect_is(w, 'zoonWorkflow',
                      info = 'The model module did not work in a standard workflow')
            
            # Chain
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = UKAnophelesPlumbeus,
                                                        covariate = UKAir,
                                                        process = list(Background(n = 20),
                                                                       Background(n = 70)),
                                                        model = ModelModule,
                                                        output = PrintMap,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'ModelModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
            
            expect_is(w, 'zoonWorkflow',
                      info = 'The model module did not work in a chain workflow')
            
            # list
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = UKAnophelesPlumbeus,
                                                        covariate = UKAir,
                                                        process = Background(n = 70),
                                                        model = list(ModelModule, ModelModule),
                                                        output = PrintMap,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'ModelModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
            
            expect_is(w, 'zoonWorkflow',
                      info = 'The model module did not work in a list workflow')
            
            # crossvalidate
            w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = UKAnophelesPlumbeus,
                                                        covariate = UKAir,
                                                        process = Chain(Background(n = 70), Crossvalidate),
                                                        model = ModelModule,
                                                        output = PerformanceMeasures,
                                                        forceReproducible = FALSE)},
                                  placeholder = 'ModelModule',
                                  fun = roxy_parse$name)
            if(inherits(w, what = 'moduleError')) stop(w)
            
            expect_is(w, 'zoonWorkflow',
                      info = 'The model module did not work in a crossvalidation workflow')
          }
          }
        }
      }
    }
  })
  
  
  test_that(paste('Check OUTPUT output formats for', basename(modulePath)),{ 
    
    ## OUTPUT MODULES ##
    if(roxy_parse$family == 'output'){
      
      Background <- LogisticRegression <- NULL
      UKAir <- UKAnophelesPlumbeus <- NULL
      
      ## Move these to the sections where they are needed
      LoadModule('Background'); LoadModule('LogisticRegression')
      LoadModule('UKAir'); LoadModule('UKAnophelesPlumbeus')
      
      # Load the script
      source(modulePath) 
      
      # Run the module with defaults
      sections <- roxy_parse[grepl('section', names(roxy_parse))]
      data_types <- gsub('Data type: ', '', sections[grepl('Data type: ', sections)])
      
      # assign the module
      OutputModule <- source(modulePath)$value
      # this line is needed to  it to the global env. where zoon looks
      assign('OutputModule', OutputModule, envir = en)
      
      
      ## ADD IN TEST OF OUTPUTS IF WE DECIDE THEY SHOULD CHAIN ##
      
      ## At the moment output modules are expected to work with 
      ## all data types
        
      # Run the module with defaults
      w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = UKAnophelesPlumbeus,
                                           covariate = UKAir,
                                           process = Background(n = 70),
                                           model = LogisticRegression,
                                           output = OutputModule)},
                            placeholder = 'OutputModule',
                            fun = roxy_parse$name)
      if(inherits(w, what = 'moduleError')) stop(w)
      
      # normal
      expect_is(w, 'zoonWorkflow',
                info = 'The output module did not work in a standard workflow')
      
      # # Chained
      # expect_is(w <- workflow(occurrence = UKAnophelesPlumbeus,
      #                         covariate = UKAir,
      #                         process = Background(n = 70),
      #                         model = LogisticRegression,
      #                         output = Chain(OutputModule, OutputModule)),
      #           'zoonWorkflow',
      #           info = 'The output module did not work in a chain workflow')
      
      # list + crossvalidated
      w <- tryCatchWorkflow(expr = {w <- workflow(occurrence = UKAnophelesPlumbeus,
                                                  covariate = UKAir,
                                                  process = Background(n = 70),
                                                  model = LogisticRegression,
                                                  output = OutputModule)},
                            placeholder = 'OutputModule',
                            fun = roxy_parse$name)
      if(inherits(w, what = 'moduleError')) stop(w)
      
      expect_is(w, 'zoonWorkflow',
                info = 'The process module did not work in a list workflow')
    }
  })
}
