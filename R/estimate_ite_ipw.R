#' @title
#' Estimate the Individual Treatment Effect using Inverse Propensity Weighting
#'
#' @description
#' Estimates the Individual Treatment Effect using Inverse Propensity Weighting
#' given a response vector, a treatment vector, and a features matrix.
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param ps_method estimation method for the propensity score
#'
#' @return a vector of ITE estimates
#'
#' @export
#'
#' @examples
#' dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
#'                                 effect_size = 2, binary = FALSE)
#'
#' # Initialize parameters
#' y <- dataset[["y"]]
#' z <- dataset[["z"]]
#' X <- as.data.frame(dataset[["X"]])
#' ps_method <- "SL.xgboost"
#'
#' ite_list <- estimate_ite_ipw(y, z, X, ps_method)
#'
estimate_ite_ipw <- function(y, z, X, ps_method) {

  est_ps <- estimate_ps(z, X, ps_method)
  ite <- ((z / est_ps) - (1 - z) / (1 - est_ps)) * y

  return(ite)
}
