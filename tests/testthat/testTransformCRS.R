# context('Transfrom CRS Function')
# 
# LoadModule('NaiveRandomPresence')
# occ_data <- NaiveRandomPresence(extent = c(0, 1000000, 0, 1000000), seed = 123) 
# tempcsv1 <- tempfile(fileext = '.csv')
# write.csv(occ_data, row.names = FALSE, file = tempcsv1)
# occ_data$crs <- '+init=epsg:27700'
# tempcsv2 <- tempfile(fileext = '.csv')
# write.csv(occ_data, row.names = FALSE, file = tempcsv2)
# 
# test_that('Fails if an invalid lat/long is used', {
#   
#   # expect error as not lat long and crs column not given
#   expect_error(workflow(occurrence = LocalOccurrenceData(tempcsv1),
#                         covariate  = UKAir,
#                         process    = Background(n=70),
#                         model      = LogisticRegression,
#                         output     = PrintMap))
#               
# })
# 
# test_that('Converts easting/northing to lat long', {
#   
#   occ_data_new <- zoon:::TransformCRS(data = occ_data, module = 'occurrence')
#     
#   expect_equal(nrow(occ_data), nrow(occ_data_new), info = 'Occurrence data has different number of rows after coordinate transformation')
#   expect_identical(names(occ_data), names(occ_data_new), 'Occurrence data has different column names after coordinate transformation')
#   
#   t1 <- occ_data_new$longitude <= 180 & occ_data_new$longitude >= -180
#   t2 <- occ_data_new$latitude <= 90 & occ_data_new$latitude >= -90
#   
#   expect_true(all(c(t1, t2)), info = 'Some transformed values are not lat/long')
#   
#   expect_is(workflow(occurrence = LocalOccurrenceData(tempcsv2),
#                         covariate  = UKAir,
#                         process    = Background(n=70),
#                         model      = LogisticRegression,
#                         output     = PrintMap),
#             class = 'zoonWorkflow')
# })
# 
# test_that('Handles NA values and blanks', {
#   
#   occ_data$latitude[1] <- NA
#   occ_data$longitude[3] <- ''
#   
#   occ_data_new <- zoon:::TransformCRS(data = occ_data, module = 'occurrence')
#   
#   expect_equal(as.numeric(attr(na.omit(occ_data_new$longitude), 'na.action')), c(1,3))
#   
# })
