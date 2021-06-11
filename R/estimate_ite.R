#' @title
#' Estimate the Individual Treatment Effect
#'
#' @description
#' Method for estimating the Individual Treatment Effect given a response vector, a treatment vector, a features matrix, and a desired algorithm
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param ite_method the method for estimating the Individual Treatment Effect, either Inverse Propensity Weighting "ipw", Standardized Inverse Propensity Weighting "sipw", Outcome Regression "or", BART "bart", Bayesian Causal Forest "bcf", or Causal Forest "cf"
#'
#' @return a list of raw ITE estimates and standardized ITE estimates
#'
#' @export
#'
#' @examples
#' TBD
estimate_ite <- function(y, z, X, ite_method){
  # TBD
  # A wrapper function to call estimate_ite_<xyz> functions.
}
