#' @title
#' Estimate the Individual Treatment Effect (ITE) using Stabilized Inverse
#' Probability Weighting (SIPW)
#'
#' @description
#' Estimates the Individual Treatment Effect using Stabilized Inverse
#' Probability Weighting given a response vector, a treatment vector, and a
#' features matrix.
#'
#' @param y An observed response vector.
#' @param z A treatment vector.
#' @param X A features matrix.
#' @param ps_method An estimation method for the propensity score.
#'
#' @return
#' A vector of ITE estimates.
#'
#' @keywords internal
#'
estimate_ite_sipw <- function(y, z, X, ps_method) {

  est_ps <- estimate_ps(z, X, ps_method)
  ite <- ((z / est_ps) / (1 / length(z) * sum(z / est_ps)) -
            (1 - z) / (1 - est_ps) /
            (1 / length(z) * sum((1 - z) / (1 - est_ps)))) * y

  return(ite)
}
