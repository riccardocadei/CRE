#' @title
#' Estimate the Individual Treatment Effect using Causal Forest
#'
#' @description
#' Estimates the Individual Treatment Effect using Causal Forest given a
#' response vector, a treatment vector, and a features matrix
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param include_ps whether or not to include propensity score estimate as a
#' covariate in ITE estimation
#'
#' @return
#' a list of ITE estimates and standard deviations for the ITE estimates
#'
#' @export
#'
estimate_ite_cf <- function(y, z, X, include_ps) {

  if (include_ps) {
    est_ps <- estimate_ps(z, X)
    X <- cbind(X, est_ps)
  }

  tau_forest <- grf::causal_forest(X, y, z)
  ite <- stats::predict(tau_forest)$predictions
  sd_ite <- sqrt(stats::predict(tau_forest,
                                estimate.variance = TRUE)$variance.estimates)

  return(list(ite, sd_ite))
}
