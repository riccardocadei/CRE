#' @title
#' Estimate the Individual Treatment Effect (ITE) using Causal Forest (CF)
#'
#' @description
#' Estimates the Individual Treatment Effect using Causal Forest given a
#' response vector, a treatment vector, and a features matrix.
#'
#' @param y An observed response vector.
#' @param z A treatment vector.
#' @param X A features matrix.
#' @param include_ps whether or not to include propensity score estimate as a
#' covariate in ITE estimation.
#' @param ps_method A method for the estimation of the propensity score.
#'
#' @return
#' A list of ITE estimates and standard deviations for the ITE estimates.
#'
#' @keywords internal
#'
estimate_ite_cf <- function(y, z, X, include_ps, ps_method) {

  if (!requireNamespace("grf", quietly = TRUE)) {
    stop(
      "Package \"grf\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (include_ps) {
    est_ps <- estimate_ps(z, X, ps_method)
    X <- cbind(X, est_ps)
  }

  tau_forest <- grf::causal_forest(X, y, z)
  ite <- stats::predict(tau_forest)$predictions
  sd_ite <- sqrt(stats::predict(tau_forest,
                                estimate.variance = TRUE)$variance.estimates)

  return(list(ite, sd_ite))
}
