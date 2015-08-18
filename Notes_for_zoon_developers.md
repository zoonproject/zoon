# Notes for zoon developers

Sorry this might be a fairly vague list. 

### 

+ We are using the google style guide.
+ We are using roxygen2 to document the code (including modules but see `BuildModules`)
+ Vignettes are written with knitr.
+ I have put myself as author in a number of places. Feel free to remove them as and when.


### Where things are
+ The code is hosted on github.com/zoonproject/zoon Modules are hosted on github.com/zonproject/modules
+ There is the google drive but that is not really used for code. https://drive.google.com/?authuser=0#folders/0BwW9iyZ_j_sXaE9yU0RMMlNZcE0
+ Issues are on github. There are a number of small issues I had that I just opened an issue for as a record.
+ Vignette source files are in zoon/vignettes. On running build_vignettes() they get copied to inst/docs/ 
+ zoonDemo.R is a quick overview of basic zoon usage. the basic-zoon-usage vignette is probably better.
+ zoonQuickStart.R just tells you which packages need installing.

### Quirks

+ The code is quite horrible because of the way modules are sourced within `workflow`. This means environments is a constant issue. 
  + To run tests do test_dir('/path/tests/testthat', env = environment(workflow))
  + However this often isn't quite the same as `R CMD check zoon`. Which is annoying.
  + This also means that running the source of `workflow` interactively is not the same as calling `workflow()`.
+ The fact that all module arguments are substituted at the beginning of `workflow` means that running `workflow` interactively requires you to define your arguments substituted. This is also annoying.
+ The package `sp` has to be in depends rather than imports for some reason. There's a stackoverflow about it somewhere.
+ Testing RerunWorkflow is quite difficult. Simulating a crash is difficult.

