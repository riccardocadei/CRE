#' @title
#' Estimate the Individual Treatment Effect using Standardized Inverse Propensity Weighting
#'
#' @description
#' Method for estimating the Individual Treatment Effect using Standardized Inverse Propensity Weighting given a response vector, a treatment vector, and a features matrix
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
estimate_ite_sipw <- function(y, z, X) {
  propscore_model <- glm(z ~ X, family = binomial)
  logit_ps <- predict(propscore_model)
  est_ps <- exp(logit_ps) / (1 + exp(logit_ps))
  ite <- ((z / est_ps) / (1 / length(z) * sum(z / est_ps)) - (1 - z) / (1 - est_ps) / (1 / length(z) * sum((1 - z) / (1 - est_ps)))) * y
  return(ite)
}
