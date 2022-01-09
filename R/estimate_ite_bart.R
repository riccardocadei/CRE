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
#' @param ps_method method for the estimation of the propensity score
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
#' include_ps <- TRUE
#' ps_method <- "SL.xgboost"
#'
#' ite_list <- estimate_ite_bart(y, z, X, include_ps, ps_method)
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
