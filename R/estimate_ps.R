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
estimate_ps <- function(z, X) {
  sl_pscore <- SuperLearner::SuperLearner(Y = z, X = X, newX = X, family = binomial(),
                                          SL.library = "SL.xgboost", cvControl = list(V=0))
  est_ps <- sl_pscore$SL.predict
  return(est_ps)
}
