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
#'
#' @return a vector of ITE estimates
#'
#' @export
#'
#' @examples
#' dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2,
#'                                      effect_size = 2, binary = FALSE)
#'
#' # Initialize parameters
#' y <- abs(dataset_cont[["y"]])
#' z <- dataset_cont[["z"]]
#' X <- as.data.frame(dataset_cont[["X"]])
#'
#' ite_list <- estimate_ite_ipw(y, z, X)
#'
estimate_ite_ipw <- function(y, z, X) {

  est_ps <- estimate_ps(z, X)
  ite <- ((z / est_ps) - (1 - z) / (1 - est_ps)) * y

  return(ite)
}
