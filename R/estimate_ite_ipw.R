#' @title
#' Estimate the Individual Treatment Effect using Inverse Propensity Weighting
#'
#' @description
#' Method for estimating the Individual Treatment Effect using Inverse Propensity Weighting given a response vector, a treatment vector, and a features matrix
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
estimate_ite_ipw <- function(y, z, X) {
  est_ps <- estimate_ps(z, X)
  ite <- ((z / est_ps) - (1 - z) / (1 - est_ps)) * y
  return(ite)
}
