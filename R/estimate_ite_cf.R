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
#' @param ps_method A method for the estimation of the propensity score.
#'
#' @return
#' A list of ITE estimates.
#'
#' @keywords internal
#'
estimate_ite_cf <- function(y, z, X, ps_method) {

  if (!requireNamespace("grf", quietly = TRUE)) {
    stop(
      "Package \"grf\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (!is.null(ps_method)) {
    est_ps <- estimate_ps(z, X, ps_method)
    X <- cbind(X, est_ps)
  }

  tau_forest <- grf::causal_forest(X, y, z)
  ite <- stats::predict(tau_forest)$predictions

  return(ite)
}
