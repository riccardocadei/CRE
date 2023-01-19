#' @title
#' Estimate the Individual Treatment Effect (ITE) using Bayesian Causal Forest
#' (BCF)
#'
#' @description
#' Estimates the Individual Treatment Effect using Bayesian Causal Forest given
#' a response vector, a treatment vector, and a features matrix.
#'
#' @param y An observed response vector.
#' @param z A treatment vector.
#' @param X A features matrix.
#' @param ps_method A method for the estimation of the propensity score.
#'
#' @return
#' A list of ITE estimates.
#'
#' @keywords internal
#'
estimate_ite_bcf <- function(y, z, X, ps_method) {

  logger::log_trace("ps_method: '{ps_method}' was selected.")

  X <- as.matrix(X)
  est_ps <- estimate_ps(z, X, ps_method)

  nburn <- 500
  nsim <- 500
  logger::log_trace("In bcf::bcf command nburn: {nburn} ",
                    "and nsim: {nsim} were used.")
  bcf_model <- bcf::bcf(y, z, X, X, est_ps, nburn = nburn, nsim = nsim)
  pd_ite <- bcf_model$tau
  ite <- apply(pd_ite, 2, mean)

  return(ite)
}
