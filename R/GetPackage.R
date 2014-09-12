# unexported function to install (if needed) and load a package.
GetPackage <- function (package) {
    if (!require(package)) {
        install.packages(package)
        library(package)
    }
}
