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
  library(SuperLearner)
  sl_pscore <- SuperLearner::SuperLearner(Y = z, X = as.data.frame(X),
                                          newX = as.data.frame(X), family = binomial(),
                                          SL.library = "SL.xgboost", cvControl = list(V=0))
  est_ps <- as.numeric(sl_pscore$SL.predict)
  return(est_ps)
}
