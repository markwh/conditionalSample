% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/momentsToDistArgs.R
\name{momentsToDistArgs}
\alias{momentsToDistArgs}
\title{Derive arguments to distribution functions from distribution moments}
\usage{
momentsToDistArgs(distr, moments)
}
\arguments{
\item{distr}{character. A distribution R recognizes (currently only supports
'norm', 'gamma', 'poisson')}

\item{moments}{a numeric vector, matrix, or data.frame with mean as column 1,
variance as column 2, etc. Currently only first 2 moments are used.}
}
\value{
list of distribution parameters for R param calls.
}
\description{
Returns a list of distribution parameters for R distribution calls.
}

