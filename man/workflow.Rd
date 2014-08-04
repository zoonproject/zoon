\name{workflow}
\alias{workflow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
workflow(ext, occurrenceFn, covariateFn, processFn, modelFn, mapFn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ext}{
%%     ~~Describe \code{ext} here~~
}
  \item{occurrenceFn}{
%%     ~~Describe \code{occurrenceFn} here~~
}
  \item{covariateFn}{
%%     ~~Describe \code{covariateFn} here~~
}
  \item{processFn}{
%%     ~~Describe \code{processFn} here~~
}
  \item{modelFn}{
%%     ~~Describe \code{modelFn} here~~
}
  \item{mapFn}{
%%     ~~Describe \code{mapFn} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (ext, occurrenceFn, covariateFn, processFn, modelFn, 
    mapFn) 
{
    getSource <- function(object) {
        paste(deparse(object), collapse = "\n")
    }
    occ <- occurrenceFn(ext)
    cov <- covariateFn(ext)
    df <- processFn(occ, cov)
    m <- modelFn(df)
    map <- mapFn(m, cov)
    bits <- sys.call()
    call <- paste0(bits[1], "(", paste(bits[-1], collapse = ", "), 
        ")")
    workflow <- paste0(paste(bits[-2], c(getSource(workflow), 
        getSource(occurrenceFn), getSource(covariateFn), getSource(processFn), 
        getSource(modelFn), getSource(mapFn)), sep = " <- ", 
        collapse = "\n\n"), paste0("\n\n", bits[2], " <- c(", 
        paste(ext, collapse = ", "), ")\n"), "\nans <- ", call, 
        collapse = "\n\n\n")
    return(list(extent = extent, occ = occ, cov = cov, df = df, 
        m = m, map = map, workflow = workflow))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
