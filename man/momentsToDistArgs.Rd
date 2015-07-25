\name{momentsToDistArgs}
\alias{momentsToDistArgs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
momentsToDistArgs(distr, moments)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distr}{
%%     ~~Describe \code{distr} here~~
}
  \item{moments}{
%%     ~~Describe \code{moments} here~~
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
function (distr, moments) 
{
    if (is.vector(moments)) 
        moments = matrix(moments, nrow = 1)
    moments = as.matrix(moments)
    switch(distr, norm = list(mean = moments[, 1], sd = sqrt(moments[, 
        2])), gamma = list(shape = moments[, 1]^2/moments[, 2], 
        scale = moments[, 2]/moments[, 1]), pois = list(lambda = moments[, 
        1]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
