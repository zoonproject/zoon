\name{processA}
\alias{processA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
processA(occ, ras)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{occ}{
%%     ~~Describe \code{occ} here~~
}
  \item{ras}{
%%     ~~Describe \code{ras} here~~
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
function (occ, ras) 
{
    require(dismo)
    if (!all(occ$type == "presence")) {
        stop("this function only works for presence-only data")
    }
    pa <- randomPoints(ras, 100)
    npres <- nrow(occ)
    npabs <- nrow(pa)
    occ_covs <- as.matrix(extract(ras, occ[, c("lon", "lat")]))
    pa_covs <- as.matrix(extract(ras, pa))
    covs <- rbind(occ_covs, pa_covs)
    df <- data.frame(value = rep(c(1, 0), c(npres, npabs)), type = rep(c("presence", 
        "background"), c(npres, npabs)), lon = c(occ$lon, pa[, 
        1]), lat = c(occ$lat, pa[, 2]), covs)
    names(df)[5:ncol(df)] <- names(ras)
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
