#' Generate samples from conditional distribution defined by a (generalized) linear model.
#'
#' @param object a lm object (including glm and gam). If a glm/gam, its family
#' must be gaussian, binomial, poisson, or gamma.
#' @param newdata As in predict.lm, predict.glm, etc. An optional data.frame to use for
#' generating conditional distribution parameters. If omitted, the fitted values are used.
#' @param vector of quantiles to which the returned values will correspond.
#' If the default, "random" is not used, this must be numeric on (0, 1).
#' @return Numeric vector containing conditional random sample (if `quantile = "random"`)
#' or conditional quantiles from condition distribution defined by `object` and `newdata`
#' @export

condlSample <- function() UseMethod("condlSample")

#' Generate samples from conditional distribution defined by a (generalized) linear model.
#'
#' @param object a lm object (including glm and gam). If a glm/gam, its family
#' must be gaussian, binomial, poisson, or gamma.
#' @param newdata As in predict.lm, predict.glm, etc. An optional data.frame to use for
#' generating conditional distribution parameters. If omitted, the fitted values are used.
#' @param vector of quantiles to which the returned values will correspond.
#' If the default, "random" is not used, this must be numeric on (0, 1).
#' @return Numeric vector containing conditional random sample (if `quantile = "random"`)
#' or conditional quantiles from condition distribution defined by `object` and `newdata`

condlSample.lm <- function(object, newdata, quantile = "random") {

  if(quantile == "random")
    quantile = runif(nrow(newdata)) else
      quantile = rep_len(quantile, nrow(newdata))

  # get distribution from family(object)
  fam = stats::family(object)$family
  fams = c("binomial", "gaussian", "Gamma", "poisson")
  distrs = c("binom", "norm", "gamma", "pois")
  distr = distrs[which(fams == fam)]
  if(identical(distr, integer(0))) stop("Specified distribution not available.")

  # get moments via predict
  preds = as.data.frame(predict(object = object, newdata = newdata, se.fit = TRUE))
  names(preds)[1] = "fit"

  # get distribution parameters via moments
  args0 = momentsToDistArgs(distr = distr, moments = preds[c("fit", "se.fit")])

  # sample from distribution
  funname = paste0("q", distr)
  out = do.call(funname, c(list(p = quantile), args0))
  out
}
#
# x = rnorm(20)
# df1 = data.frame(x = x, y = x + rnorm(20))
# lm1 = lm(y ~ x, df1)
# condlSample.lm(lm1, data.frame(x = c(3, 7)), q = 0.99)
# condlSample.lm
