# Zoön

Zoön is a package for the reproducible and shareable analysis of species distribution models with a focus on the ability to compare between models and diagnostic output of models.

An overview of the project can be found [here](http://www.2020science.net/research/species-distribution-modelling).
There is a blog to keep collaborators up to date with progress. This can be found [here](http://zoonproject.wordpress.com)

Zoön is still being developed. Feel free to clone and use the code, open issues, let us know what you want etc. But don't expect much functionality from the package yet. If you would like to add functionality, please start writing modules!

### To install the stable(ish) version directly from R

You'll need to be using **R version 3.2.0** or higher.

On Windows:

```r
# install the Windows binary of the latest release
install.packages('https://github.com/zoonproject/zoon/releases/download/0.3.1/zoon_0.3.1.zip',
                repos = NULL, method = 'libcurl')

# install dependencies
install.packages(pkgDepends('zoon')$Depends)

# install imports
install.packages(unlist(strsplit(gsub(',', '', installed.packages(fields = c('imports'))['zoon','Imports']), '\n'))[-1])

# load zoon
library(zoon)
```

and on OSX or Linux:

```r
# install the Windows binary of the latest release
install.packages('https://github.com/zoonproject/zoon/releases/download/0.3.1/zoon_0.3.1.tar.gz',
                repos = NULL, method = 'libcurl')

# install dependencies
install.packages(pkgDepends('zoon')$Depends)

# install imports
install.packages(unlist(strsplit(gsub(',', '', installed.packages(fields = c('imports'))['zoon','Imports']), '\n'))[-1])

# load zoon
library(zoon)
```



### To install current development version

If you're using Windows, you'll need to have [RTools](https://cran.r-project.org/bin/windows/Rtools/) installed first

```coffee
install.packages("devtools")
library("devtools")

install_github("zoonproject/zoon")
library("zoon")
```

### Basic usage

```coffee
# Run a workflow, specifying one module of each type.
work1 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate  = UKAir,
                  process    = OneHundredBackground,
                  model      = LogisticRegression,
                  output     = PrintMap)

# Get a list of modules (requires browser verification)
GetModuleList()

# Get help on a module
ModuleHelp(LogisticRegression)
```


### Contributing modules

Zoön has a modular structure, and we are hoping for user submitted modules. This allows Zoön to keep up to date with the fast-moving SDM field in a way a package maintained by a small team of developers can't. Modules are simple R scripts containing a single function and some metadata. They are currently kept [here](https://github.com/zoonproject/modules). The inputs and outputs of each module type are controlled. A brief description can be found [here](https://github.com/zoonproject/zoon/blob/master/vignettes/Module_IO_for_devs.Rmd). The function `BuildModule` is used to turn a function in an R session into a module. 

Please note, Zoön is still being developed. We would love you to contribute modules, but can't yet guarantee that there won't be major changes that might break modules. We will try to fix user submitted modules if we break them. 


### Notes for collaborators

We are using the [Google style guide](https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml) with the exception that function description goes before the function name, not inside the function definition. We are using [roxygen2](http://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) to document the package. Try to keep function names as verbs.


![Zoon banner](https://github.com/zoonproject/blog/blob/master/zoon.jpg)
