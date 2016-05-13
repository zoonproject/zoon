# A function for testing the outputs conform to the expected
# (within a context)
test_outputs <- function(roxy_parse, modulePath){

  test_that('Check output formats',{ 
    
    # download.file sends tonnes of stuff to my console
    # it is used internally in a number of modules
    # This is a naughty fix to shut it up (I tried everything)
    formals(download.file)$quiet <- TRUE
  
    ## OCCURRENCE MODULES ##
    if(roxy_parse$family == 'occurrence'){
      
      # Load the script
      source(modulePath) 
      
      # Run the module with defaults
      suppressWarnings({
      occ_return <- do.call(roxy_parse$name, args = list())
      })
      
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

    }
    
    ## COVARIATE MODULES ##
    if(roxy_parse$family == 'covariate'){
      
      # Load the script
      source(modulePath) 
      
      # Run the module with defaults
      suppressWarnings({
      cov_return <- do.call(roxy_parse$name, args = list())
      })
      
      # Check projection
      expect_true(all(grepl("+proj=longlat", projection(cov_return)),
                      grepl("+ellps=WGS84", projection(cov_return))),
                  info = 'Covariate module output must have WGS84 projection: proj4 string is expected to contain the elements "+proj=longlat" and "+ellps=WGS84"')
      
      # Check raster returned is as expected
      expect_is(cov_return, c('RasterLayer', 'RasterStack', 'RasterBrick'), info = 'Covariate module output must be either a RasterLayer or a RasterStack')
      
    }
    
    ## PROCESS MODULES ##
    if(roxy_parse$family == 'process'){
      
      # Load the script
      source(modulePath) 
      
      # Run the module with defaults
      sections <- roxy_parse[grepl('section', names(roxy_parse))]
      data_types <- gsub('Data type: ', '', sections[grepl('Data type: ', sections)])
      
      for(data_type in c("presence/absence", "presence-only")){
        
        if(grepl(data_type, data_types)){
          
         # visible binding
         .data <- NULL  
          
         if(data_type == "presence/absence") load(system.file("extdata", "data_PA.rdata", package="zoon"))
         if(data_type == "presence-only") load(system.file("extdata", 'data_PO.rdata', package="zoon"))
          
          suppressWarnings({
            pro_return <- do.call(roxy_parse$name, args = list(.data = .data))
          })

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
          
        }
      }
    }
  
    ## MODEL MODULES ##
    if(roxy_parse$family == 'model'){
      
      # Load the script
      source(modulePath) 
      
      # Run the module with defaults
      sections <- roxy_parse[grepl('section', names(roxy_parse))]
      data_types <- gsub('Data type: ', '', sections[grepl('Data type: ', sections)])
      
      if(length(data_types) > 0){
      
        # There are not any true presence only models as yet
        for(data_type in c("presence/absence", "presence/background")){
        
        if(grepl(data_type, data_types)){
          
          if(data_type == "presence/absence") load(system.file("extdata", 'data_PA.rdata', package="zoon"))
          if(data_type == "presence/background") load(system.file("extdata", 'data_PB.rdata', package="zoon")) # basically the same as above but type == 'background'
          
          suppressWarnings({
            mod_return <- do.call(roxy_parse$name, args = list(.df = .data$df))
          })
          
          ## Check mod_return structure
          expect_is(mod_return, 'zoonModel', info = "The object returned from a model module must be a 'zoonModel' object. See '?ZoonModel' for more details")
          expect_named(mod_return, expected = c('model', 'code', 'packages'), info = "The elements of a 'zoonModel' object must be named 'model', 'code', and 'packages'")
          
          
          ## Check 'model'
          # There are not any requirements other than that is exists
          
          
          ## Check 'code'
          # code is a charachter and can be parsed and checked
          expect_is(mod_return$code, 'character', info = 'The "code" element returned by a model module must be a character (see ?ZoonModel for details)')
          
          newdata <- as.data.frame(.data$df[, 6:ncol(.data$df)])
          names(newdata) <- names(.data$df)[6:ncol(.data$df)]
          
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
          }
        }
      }
    }
  })
}
