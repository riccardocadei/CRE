#' @title
#' Estimate the Individual Treatment Effect
#'
#' @description
#' Method for estimating the Individual Treatment Effect given a response vector, a treatment vector, a features matrix, and a desired algorithm
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param ite_method the method for estimating the Individual Treatment Effect, either Inverse Propensity Weighting "ipw", Stabilized Inverse Propensity Weighting "sipw", Outcome Regression "or", BART "bart", Bayesian Causal Forest "bcf", or Causal Forest "cf"
#'
#' @return a list of raw ITE estimates and standardized ITE estimates
#'
#' @export
#'
#' @examples
#' TBD
#'
estimate_ite <- function(y, z, X, ite_method) {
  stopifnot(ite_method %in% c("ipw", "sipw", "or", "bart", "bcf", "cf"))
  if (ite_method == "ipw") {
    ite <- CRE::estimate_ite_ipw(y, z, X)
  } else if (ite_method == "sipw") {
    ite <- CRE::estimate_ite_sipw(y, z, X)
  } else if (ite_method == "or") {
    ite <- CRE::estimate_ite_or(y, z, X)
  } else if (ite_method == "bart") {
    ite <- CRE::estimate_ite_bart(y, z, X)
  } else if (ite_method == "bcf") {
    ite <- CRE::estimate_ite_bcf(y, z, X)
  } else {
    ite <- CRE::estimate_ite_cf(y, z, X)
  }
  mu_ite <- mean(ite)
  sd_ite <- sd(ite)
  ite_std <- (ite - mu_ite) / sd_ite
  return(list(ite = ite, ite_std = ite_std))
}
