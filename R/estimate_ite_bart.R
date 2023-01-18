#' @title
#' Estimate the Individual Treatment Effect (ITE) using Bayesian Additive
#' Regression Trees (BART)
#'
#' @description
#' Estimates the Individual Treatment Effect using Bayesian Additive Regression
#' Trees given a response vector, a treatment vector, and a features matrix.
#'
#' @param y An observed response vector.
#' @param z A treatment vector.
#' @param X A features matrix.
#' @param ps_method Method for the estimation of the propensity score.
#'
#' @return A list of ITE estimates.
#'
#' @note The number of samples and the number of burn are set by default equal
#' to 500.
#'
#' @keywords internal
#'
estimate_ite_bart <- function(y, z, X, ps_method) {

  if (!is.null(ps_method)) {
    est_ps <- estimate_ps(z, X, ps_method)
    X <- cbind(X, est_ps)
  }

  bart_fit <- bartCause::bartc(as.matrix(y), as.matrix(z), as.matrix(X),
                               n.samples = 500, n.burn = 500)

  pd_ite <- bartCause::extract(bart_fit, type = "ite")
  ite <- apply(pd_ite, 2, mean)
  return(ite)
}
