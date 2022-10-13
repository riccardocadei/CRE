#' @title
#' Estimate the Individual Treatment Effect using Bayesian Causal Forest
#'
#' @description
#' Estimates the Individual Treatment Effect using Bayesian Causal Forest given
#' a response vector, a treatment vector, and a features matrix.
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param ps_method method for the estimation of the propensity score
#'
#' @return a list of ITE estimates and standard deviations for the ITE estimates
#'
#' @keywords internal
#'

estimate_ite_bcf <- function(y, z, X, ps_method) {
  est_ps <- estimate_ps(z, X, ps_method)
  bcf_model <- bcf::bcf(y, z, X, X, est_ps, nburn = 500, nsim = 500)
  pd_ite <- bcf_model$tau
  ite <- apply(pd_ite, 2, mean)
  sd_ite <- apply(pd_ite, 2, stats::sd)
  return(list(ite, sd_ite))
}
