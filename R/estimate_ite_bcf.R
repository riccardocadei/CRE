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
#' @param method_ps method for the estimation of the propensity score
#'
#' @return a list of ITE estimates and standard deviations for the ITE estimates
#'
#' @export
#'
#' @examples
#' dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
#'                                      effect_size = 2, binary = FALSE)
#'
#' # Initialize parameters
#' y <- dataset[["y"]]
#' z <- dataset[["z"]]
#' X <- as.data.frame(dataset[["X"]])
#' method_ps <- "SL.xgboost"
#' ite_list <- estimate_ite_bcf(y, z, as.matrix(X), method_ps)
#'
estimate_ite_bcf <- function(y, z, X, method_ps) {
  est_ps <- estimate_ps(z, X, method_ps)
  bcf_model <- bcf::bcf(y, z, X, X, est_ps, nburn = 500, nsim = 500)
  pd_ite <- bcf_model$tau
  ite <- apply(pd_ite, 2, mean)
  sd_ite <- apply(pd_ite, 2, stats::sd)
  return(list(ite, sd_ite))
}
