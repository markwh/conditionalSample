\name{condlSample.lm}
\alias{condlSample.lm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
condlSample.lm(object, newdata, quantile = "random")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{newdata}{
%%     ~~Describe \code{newdata} here~~
}
  \item{quantile}{
%%     ~~Describe \code{quantile} here~~
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
function (object, newdata, quantile = "random") 
{
    if (quantile == "random") 
        quantile = runif(nrow(newdata))
    else quantile = rep_len(quantile, nrow(newdata))
    fam = stats::family(object)$family
    fams = c("binomial", "gaussian", "Gamma", "poisson")
    distrs = c("binom", "norm", "gamma", "pois")
    distr = distrs[which(fams == fam)]
    if (identical(distr, integer(0))) 
        stop("Specified distribution not available.")
    preds = as.data.frame(predict(object = object, newdata = newdata, 
        se.fit = TRUE))
    names(preds)[1] = "fit"
    args0 = momentsToDistArgs(distr = distr, moments = preds[c("fit", 
        "se.fit")])
    funname = paste0("q", distr)
    out = do.call(funname, c(list(p = quantile), args0))
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
