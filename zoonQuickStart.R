
# Install zoon from github. This requires devtools
# Devtools needs Rtools if youre using windows
# http://cran.r-project.org/bin/windows/Rtools/

install.packages("devtools")
library(devtools)

install_github("zoonproject/zoon")
library(zoon)


# Some packages that are needed later
# There is actually quite a lot in here. Sorry.
install.packages(c("dismo", "randomForest", "biomod2", "RNCEP", "spocc"))

# Run this to test things are working.
workflow("UKAnophelesPlumbeus", "UKAir", "OneHundredBackground",
         "LogisticRegression", "SameTimePlaceMap")


# If you wish to use biomod2 during the workshop check that this works.
# I am having problems getting it to work on this windows machine
# But had no problems on linux.
workflow("UKAnophelesPlumbeus", "UKAir", "OneHundredBackground",
         ModuleOptions("BiomodModel", modelType="GAM"), "SameTimePlaceMap")


# Finally, this is just a useful function
# I didnt realise until recently that you needed to sign in to github to use it
# Type "yes", then check your browser. If you do not have a github account
# use username: zoonworkshop password: zoonsdm1

GetModuleList()