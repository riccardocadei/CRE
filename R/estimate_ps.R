#' @title
#' Estimate the Propensity Score
#'
#' @description
#' Method for estimating the Propensity Score given a treatment vector and
#' features data.frame.
#'
#' @param z The treatment vector.
#' @param X The features data frame.
#' @param ps_method The estimation model for the propensity score
#' (default: SL.xgboost).
#'
#' @return
#' A vector of propensity score estimates.
#'
#' @import SuperLearner
#'
#'
estimate_ps <- function(z, X, ps_method = "SL.xgboost") {
  sl_pscore <- SuperLearner(Y = z, X = as.data.frame(X),
                                          newX = as.data.frame(X),
                                          family = binomial(),
                                          SL.library = ps_method,
                                          cvControl = list(V=0))
  est_ps <- as.numeric(sl_pscore$SL.predict)
  return(est_ps)
}
