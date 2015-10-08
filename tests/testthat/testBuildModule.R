context('BuildModule')

dir <- tempdir()
dir.create(dir, showWarnings = FALSE)

NewModule <- function(.df){}

# BuildModule(object = NewModule,
#             type = 'model',
#             title = 'test',
#             description = 'test',
#             author = 'tom',
#             email = 'tom@tom.com',
#             dir = dir) 

test_that('Simplest case should build', {
  
  BuildModule(object = NewModule,
              type = 'model',
              title = 'test',
              description = 'test',
              author = 'tom',
              email = 'tom@tom.com',
              dir = dir) 
  
  expect_true(file.exists(file.path(dir, 'NewModule.r')))
  
})

test_that('All metadata given, and correct', {

  expect_error(BuildModule(object = NewModule,
                           type = '',
                           title = 'test',
                           description = 'test',
                           author = 'tom',
                           email = 'tom@tom.com',
                           dir = dir),
               'type must be one of')
  expect_error(BuildModule(object = NewModule,
                           type = 'zoon',
                           title = 'test',
                           description = 'test',
                           author = 'tom',
                           email = 'tom@tom.com',
                           dir = dir),
               'type must be one of')
  expect_warning(BuildModule(object = NewModule,
                           type = 'model',
                           title = '',
                           description = 'test',
                           author = 'tom',
                           email = 'tom@tom.com',
                           dir = dir),
               'Information not complete')  
  expect_warning(BuildModule(object = NewModule,
                             type = 'model',
                             title = 'test',
                             description = '',
                             author = 'tom',
                             email = 'tom@tom.com',
                             dir = dir),
                 'Information not complete') 
  expect_warning(BuildModule(object = NewModule,
                             type = 'model',
                             title = 'test',
                             description = 'test',
                             author = '',
                             email = 'tom@tom.com',
                             dir = dir),
                 'Information not complete') 
  expect_warning(BuildModule(object = NewModule,
                             type = 'model',
                             title = 'test',
                             description = 'test',
                             author = 'tom',
                             email = '',
                             dir = dir),
                 'Information not complete')
  
})

  
test_that('Misc tests', {
  
  expect_error(BuildModule(object = NewModule,
                             type = 'model',
                             title = 'test',
                             description = 'test',
                             author = 'tom',
                             email = 'tom@tom.com',
                             dir = 'thisdoesnotexist'),
                 'directory is not writeable')
  
})

unlink(x = dir, recursive = TRUE)