#' @title
#' Estimate the Individual Treatment Effect using Bayesian Causal Forest
#'
#' @description
#' Method for estimating the Individual Treatment Effect using Bayesian Causal Forest given a response vector, a treatment vector, and a features matrix
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#'
#' @return a vector of ITE estimates
#'
#' @export
#'
#' @examples
#' TBD
#'
estimate_ite_bcf <- function(y, z, X) {
  propscore_model <- stats::glm(z ~ X, family = binomial)
  logit_ps <- stats::predict(propscore_model)
  est_ps <- exp(logit_ps) / (1 + exp(logit_ps))
  bcf_model <- bcf::bcf(y, z, X, X, est_ps, nburn = 100, nsim = 1000)
  ite <- colMeans(bcf_model$tau)
  return(ite)
}
