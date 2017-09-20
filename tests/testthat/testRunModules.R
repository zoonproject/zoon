context("RunModules")

# Having environment problems with this test.
# test_that('RunModels function works correctly', {
#  skip_on_cran()

#  GetModule('LogisticRegression', forceReproducible = FALSE)

#  e <- environment()

#  # Test data with training and external validation data
#
#  df <- data.frame(value = rep(c(0,1), 10),
#                   type = rep(c('absence', 'presence'), 10),
#                   lon = 1:20, lat = 1:20,
#                   fold = rep(c(1,0), each = 10),
#                   cov1 = 1:20)


#  x <- RunModels(df, 'LogisticRegression', list(), e)

#  expect_equal(class(x$data), 'data.frame')
#  expect_equal(class(x$model), c('glm', 'lm'))
#  expect_true('predictions' %in% names(x$data))
#  # As no crossvalidation, training data should not have predictions
#  expect_true(all(is.na(x$data[x$data['fold'] == 1,'predictions'])))

#  # The final model should be trained on non external validation data.
#  expect_true(length(x$model$y) == 10)




#  # Test data with crossvalidation data
#
#  df2 <- data.frame(value = rep(c(0,1), 10),
#                   type = rep(c('absence', 'presence'), 10),
#                   lon = 1:20, lat = 1:20,
#                   fold = rep(c(1,2), each = 10),
#                   cov1 = c(1:10, 1:10))


#  x2 <- RunModels(df2, 'LogisticRegression', list(), e)

#  expect_equal(class(x2$data), 'data.frame')
#  expect_equal(class(x2$model), c('glm', 'lm'))
#  expect_true('predictions' %in% names(x2$data))

#  # As crossvalidation, all data should have predictions
#  expect_true(all(!is.na(x2$data[,'predictions'])))

#  # The final model should be trained on all data.
#  expect_true(length(x2$model$y) == 20)

#  # Because cov1 is artificially replicated, predictions should be equal
#  expect_true(all.equal(x2$data[1:10,'predictions'],
#                        x2$data[11:20,'predictions']))


#  # Mix of external and cross validation
#  df3 <- data.frame(value = rep(c(0,1), 10),
#                   type = rep(c('absence', 'presence'), 10),
#                   lon = 1:20, lat = 1:20,
#                   fold = c(rep(c(1,2), each = 5), rep(0,10)),
#                   cov1 = c(1:10, 1:10))


#  x3 <- RunModels(df3, 'LogisticRegression', list(), e)

#  expect_equal(class(x3$data), 'data.frame')
#  expect_equal(class(x3$model), c('glm', 'lm'))
#  expect_true('predictions' %in% names(x3$data))

#  # All data should have predictions, cross validated data from CV and
#  #   and external validation from full model
#  expect_true(all(!is.na(x3$data[,'predictions'])))

#  # The final model should be trained on half data.
#  expect_true(length(x3$model$y) == 10)


# })
