#' @title
#' Estimate the Individual Treatment Effect using Inverse Propensity Weighting
#'
#' @description
#' Estimates the Individual Treatment Effect (ITE) using Inverse Propensity
#' Weighting (IPW) given a response vector, a treatment vector, and a features
#' matrix.
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param ps_method estimation method for the propensity score
#'
#' @return a vector of ITE estimates
#'
#' @keywords internal
#'

estimate_ite_ipw <- function(y, z, X, ps_method) {

  est_ps <- estimate_ps(z, X, ps_method)
  ite <- ((z / est_ps) - (1 - z) / (1 - est_ps)) * y

  return(ite)
}
