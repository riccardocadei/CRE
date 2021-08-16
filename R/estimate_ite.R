#' @title
#' Estimate the Individual Treatment Effect
#'
#' @description
#' Method for estimating the Individual Treatment Effect given a response vector, a treatment vector, a features matrix, and a desired algorithm
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param ite_method the method for estimating the Individual Treatment Effect, either Inverse Propensity Weighting "ipw", Stabilized Inverse Propensity Weighting "sipw", Outcome Regression "or", BART "bart", Accelerated BART "xbart", Bayesian Causal Forest "bcf", Accelerated Bayesian Causal Forest "xbcf", or Causal Forest "cf"
#' @param include_ps whether or not to include propensity score estimate as a covariate in ITE estimation
#' @param binary whether or not the outcome is binary
#'
#' @return a list of raw ITE estimates, standardized ITE estimates, and standard deviations for the ITE estimates
#'
#' @export
#'
estimate_ite <- function(y, z, X, ite_method, include_ps, binary) {
  if (ite_method == "ipw") {
    ite <- estimate_ite_ipw(y, z, X)
    sd_ite <- NA
  } else if (ite_method == "sipw") {
    ite <- estimate_ite_sipw(y, z, X)
    sd_ite <- NA
  } else if (ite_method == "or") {
    ite <- estimate_ite_or(y, z, X)
    sd_ite <- NA
  } else if (ite_method == "bart") {
    ite_results <- estimate_ite_bart(y, z, X, include_ps)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "xbart") {
    ite_results <- estimate_ite_xbart(y, z, X, include_ps)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "bcf") {
    ite_results <- estimate_ite_bcf(y, z, X)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "xbcf") {
    ite_results <- estimate_ite_xbcf(y, z, X)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "cf") {
    ite_results <- estimate_ite_cf(y, z, X, include_ps)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else {
    stop("Invalid ITE method. Please choose from the following:
         'ipw', 'sipw', or, 'bart', 'xbart', 'bcf', 'xbcf', or 'cf'")
  }
  if (binary) {
    ite <- round(ite, 0)
  }
  ite_std <- (ite - mean(ite)) / stats::sd(ite)
  return(list(ite = ite, ite_std = ite_std, sd_ite = sd_ite))
}
