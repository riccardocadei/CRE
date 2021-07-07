#' @title
#' Estimate the Propensity Score
#'
#' @description
#' Method for estimating the Propensity Score given a treatment vector and features matrix
#'
#' @param z the treatment vector
#' @param X the features matrix
#'
#' @return a list of propensity score estimates
#'
#' @export
#'
#' @examples
#' TBD
#'
estimate_ps <- function(z, X) {
  propscore_model <- stats::glm(z ~ X, family = binomial)
  logit_ps <- stats::predict(propscore_model)
  est_ps <- exp(logit_ps) / (1 + exp(logit_ps))
  return(est_ps)
}
