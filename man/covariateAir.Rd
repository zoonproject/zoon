\name{covariateAir}
\alias{covariateAir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
covariateAir(extent)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{extent}{
%%     ~~Describe \code{extent} here~~
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
function (extent) 
{
    require(RNCEP)
    c1 <- NCEP.gather(variable = "air", level = 850, months.minmax = c(1:2), 
        years.minmax = c(2000, 2001), lat.southnorth = extent[3:4], 
        lon.westeast = extent[1:2], reanalysis2 = FALSE, return.units = TRUE)
    avg <- apply(c1, c(1, 2), mean)
    ras <- raster(avg)
    extent(ras) <- c(extent)
    return(ras)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
