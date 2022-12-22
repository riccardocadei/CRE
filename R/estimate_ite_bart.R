#' @title
#' Estimate the Individual Treatment Effect using Bayesian Additive Regression
#' Trees
#'
#' @description
#' Estimates the Individual Treatment Effect using Bayesian Additive Regression
#' Trees given a response vector, a treatment vector, and a features matrix.
#'
#' @param y The observed response vector.
#' @param z The treatment vector.
#' @param X The features matrix.
#' @param include_ps Whether or not to include propensity score estimate as a
#' covariate in ITE estimation.
#' @param ps_method Method for the estimation of the propensity score.
#'
#' @return A list of ITE estimates and standard deviations for the ITE
#' estimates.
#'
#' @note The number of samples and the number of burn are set by default equal
#' to 500.
#'
#' @keywords internal
#'
estimate_ite_bart <- function(y, z, X, include_ps, ps_method) {

  if (include_ps) {
    est_ps <- estimate_ps(z, X, ps_method)
    X <- cbind(X, est_ps)
  }

  bart_fit <- bartCause::bartc(as.matrix(y), as.matrix(z), as.matrix(X),
                               n.samples = 500, n.burn = 500)

  pd_ite <- bartCause::extract(bart_fit, type = "ite")
  ite <- apply(pd_ite, 2, mean)
  sd_ite <- apply(pd_ite, 2, stats::sd)
  return(list(ite, sd_ite))
}
