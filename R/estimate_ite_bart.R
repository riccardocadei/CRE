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

  logger::log_trace("ps_method: '{ps_method}' was selected.")

  if (!is.null(ps_method)) {
    est_ps <- estimate_ps(z, X, ps_method)
    X <- cbind(X, est_ps)
  }

  n_sample <- 500
  n_burn <- 500
  logger::log_trace("In bartCause::bartc command n.samples: {n_sample} ",
                    "and n.burn: {n_burn} were used.")

  bart_fit <- bartCause::bartc(as.matrix(y), as.matrix(z), as.matrix(X),
                               n.samples = n_sample, n.burn = n_burn)

  pd_ite <- bartCause::extract(bart_fit, type = "ite")
  ite <- apply(pd_ite, 2, mean)

  return(ite)
}
