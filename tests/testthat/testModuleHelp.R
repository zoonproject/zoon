context('ModuleHelp')

directory <- tempdir()

test_that('ModuleHelp errors', {
  
  if (!capabilities('libcurl')) skip('skipping as libcurl not supported')  
  
  expect_error(ModuleHelp(c(one, two)),
               "module must be a character of length 1")
  
  expect_error(ModuleHelp('NoProce1s'),
               "Can't find 'NoProce1s'. Did you mean")
  
  org <- getOption('help_type')
  options(help_type = 'text')
  
  capture.output(ModuleHelp('NoProcess'),
                 file = file.path(directory, 'help.txt'))
  
  expect_true(file.exists(file.path(directory, 'help.txt')))
  unlink(x = file.path(directory, 'help.txt'))
  
  options(help_type = org)


  
  # Some modules with no close matches
  expect_error(ModuleHelp('zzzzz123232'), 'or any modules with closely matching names')
  
  # Module with 1 close matches
  expect_error(ModuleHelp('NoProcesss'), "Can't find 'NoProcesss'. Did you mean")

  # Module with 2 close matches
  expect_error(ModuleHelp('Crissvalid'), "Can't find 'Crissvalid'. Did you mean one of")
})


