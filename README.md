# Zoon

Zoon is a package for the reproducible and shareable analysis of species distribution models with a focus on the ability to compare between models and diagnostic output of models.


An overview of the project can be found [here](http://www.2020science.net/research/species-distribution-modelling).


Zoon is just starting to be developed. Feel free to clone and use the code, open issues, let us know what you want etc. But don't expect much functionality from the package yet.



### To install current development version.

```coffee
install.packages("devtools")
library("devtools")

install_github("zoonproject/zoon")
library("zoon")
```


### Notes for collaborators

We are using the [Google style guide](https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml) with the exception that function description goes before the function name, not inside the function definition. We are using [roxygen2](http://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) to document the package. Try to keep function names as verbs.


