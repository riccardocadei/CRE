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
#' @param include_ps whether or not to include propensity score estimate as a covariate in ITE estimation
#'
#' @return a list of raw ITE estimates and standardized ITE estimates
#'
#' @export
#'
#' @examples
#' TBD
#'
estimate_ite <- function(y, z, X, ite_method, include_ps) {
  if (ite_method == "ipw") {
    ite <- estimate_ite_ipw(y, z, X)
  } else if (ite_method == "sipw") {
    ite <- estimate_ite_sipw(y, z, X)
  } else if (ite_method == "or") {
    ite <- estimate_ite_or(y, z, X)
  } else if (ite_method == "bart") {
    ite <- estimate_ite_bart(y, z, X, include_ps)
  } else if (ite_method == "xbart") {
    ite <- estimate_ite_xbart(y, z, X, include_ps)
  } else if (ite_method == "bcf") {
    ite <- estimate_ite_bcf(y, z, X)
  } else if (ite_method == "xbcf") {
    ite <- estimate_ite_xbcf(y, z, X)
  } else if (ite_method == "cf") {
    ite <- estimate_ite_cf(y, z, X, include_ps)
  } else {
    stop("Invalid ITE method. Please choose from the following:
         'ipw', 'sipw', or, 'bart', 'xbart', 'bcf', 'xbcf', or 'cf'")
  }
  ite_std <- (ite - mean(ite)) / sd(ite)
  return(list(ite = ite, ite_std = ite_std))
}
