#' @title
#' Estimate the Individual Treatment Effect using Causal Forest
#'
#' @description
#' Method for estimating the Individual Treatment Effect using Causal Forest given a response vector, a treatment vector, and a features matrix
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
#' TBD
#'
estimate_ite_cf <- function(y, z, X) {
  tau_forest <- causal_forest(X, y, z)
  ite <- predict(tau_forest)$predictions
  return(ite)
}
