#' @title
#' Estimate the Individual Treatment Effect using Bayesian Additive Regression Trees
#'
#' @description
#' Estimates the Individual Treatment Effect using Bayesian Additive Regression
#' Trees given a response vector, a treatment vector, and a features matrix.
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param include_ps whether or not to include propensity score estimate as a
#' covariate in ITE estimation
#'
#' @return a list of ITE estimates and standard deviations for the ITE estimates
#'
#' @export
#'
estimate_ite_bart <- function(y, z, X, include_ps) {
  if (include_ps) {
    est_ps <- estimate_ps(z, X)
    X <- cbind(X, est_ps)
  }
  bart_fit <- bartCause::bartc(as.matrix(y), as.matrix(z), as.matrix(X),
                               n.samples = 500, n.burn = 500)
  pd_ite <- bartCause::extract(bart_fit, type = "ite")
  ite <- apply(pd_ite, 2, mean)
  sd_ite <- apply(pd_ite, 2, stats::sd)
  return(list(ite, sd_ite))
}
