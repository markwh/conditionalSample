#' Derive arguments to distribution functions from distribution moments
#'
#' Returns a list of distribution parameters for R distribution calls.
#'
#' @param distr character. A distribution R recognizes (currently only supports
#' "norm", "gamma", "poisson")
#' @param moments a numeric vector, matrix, or data.frame with mean as column 1,
#' variance as column 2, etc. Currently only first 2 moments are used.
#' @return list of distribution parameters for R param calls.
#' @export

momentsToDistArgs = function(distr, moments){
  if(is.vector(moments)) moments = matrix(moments, nrow = 1)
  moments = as.matrix(moments)
  switch(distr,
         norm = list(mean = moments[,1], sd = sqrt(moments[,2])),
         gamma = list(shape = moments[,1] ^ 2 / moments[,2],
                   scale = moments[,2] / moments[,1]),
         pois = list(lambda = moments[,1]))
}


# foo = momentsToDistArgs("norm", c(0, 4))
# bar = momentsToDistArgs("gamma", data.frame(means = runif(10, 1, 10), vars = runif(10, 10, 20)))
# do.call("rnorm", c(list(n = 10), foo))
# do.call("rgamma", c(list(n = 10), bar))
#
#
