![Zoon banner](https://github.com/zoonproject/blog/blob/master/zoon_top.png)

[![Build Status](https://travis-ci.org/zoonproject/zoon.svg)](https://travis-ci.org/zoonproject/zoon)
[![codecov.io](https://codecov.io/github/zoonproject/zoon/coverage.svg?branch=master)](https://codecov.io/github/zoonproject/zoon?branch=master)

Zoön is a package for the reproducible and shareable analysis of species distribution models with a focus on the ability to compare between models and diagnostic output of models.

An overview of the project can be found [here](http://www.2020science.net/research/species-distribution-modelling).
There is a blog to keep collaborators up to date with progress. This can be found [here](http://zoonproject.wordpress.com)

Zoön is still being developed. Feel free to clone and use the code, open issues, let us know what you want etc. But don't expect much functionality from the package yet. If you would like to add functionality, please start writing modules!

### Basic usage

```r
library(zoon)

# Run a workflow, specifying one module of each type.
work1 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate  = UKAir,
                  process    = OneHundredBackground,
                  model      = LogisticRegression,
                  output     = PrintMap)

# Get a list of modules
GetModuleList()

# Get help on a module
ModuleHelp(LogisticRegression)
```


### To install the stable(ish) version directly from R

Zoon isn't on CRAN yet, but you can install the package with the following instructions. 
Note, you'll need to be using **R version 3.2.0** or higher.

First, install some dependencies:
```r
install.packages(c("raster", "sp (>= 1.0-13)", "RCurl", "dismo"))
```

Then to install on Windows do:

```r
install.packages('https://github.com/zoonproject/zoon/releases/download/0.3.2/zoon_0.3.2.zip',
                repos = NULL, method = 'libcurl')
```

and on OSX or Linux:

```r
install.packages('https://github.com/zoonproject/zoon/releases/download/0.3.2/zoon_0.3.2.tar.gz',
                 repos = NULL, method = 'libcurl')
```


### To install current development version

You can install the most recent version of the package straight from GitHub using the `devtools` package,
though if you're using Windows, you'll need to have [RTools](https://cran.r-project.org/bin/windows/Rtools/) installed first:

```r
devtools::install_github("zoonproject/zoon")
```

### Contributing modules

Zoön has a modular structure, and we are hoping for user submitted modules. This allows Zoön to keep up to date with the fast-moving SDM field in a way a package maintained by a small team of developers can't. Modules are simple R scripts containing a single function and some metadata. They are currently kept [here](https://github.com/zoonproject/modules). The inputs and outputs of each module type are controlled. A brief description can be found [here](https://rawgit.com/zoonproject/zoon/master/inst/doc/Module_IO_for_devs.html). The function `BuildModule` is used to turn a function in an R session into a module. 

Please note, Zoön is still being developed. We would love you to contribute modules, but can't yet guarantee that there won't be major changes that might break modules. We will try to fix user submitted modules if we break them. 


### Notes for collaborators

We welcome collaboration and input anyone who'd like to get involved!
If you have any comments, suggestions or you spot any bugs or errors, please let us know via [the issue tracker](https://github.com/zoonproject/zoon/issues).
Pull requests are always welcome, though please let us know what you're developing first so we plan how to integrate it into the main package. 

We are committed to making Zoön an inclusive project that the whole research community can contribute to and benefit from it and ask all contributers (including the Zoön development team) to stick to [a code of conduct](https://github.com/zoonproject/zoon/blob/master/code_of_conduct.md)

We are using the [Google style guide](https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml) with the exception that function description goes before the function name, not inside the function definition. We are using [roxygen2](http://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) to document the package. Try to keep function names as verbs.


![Zoon banner](https://github.com/zoonproject/blog/blob/master/zoon.jpg)
