#' @title
#' Estimate the propensity score
#'
#' @description
#' Estimates the Propensity Score given a treatment vector and
#' features data frame.
#'
#' @param z A treatment vector.
#' @param X A features data frame.
#' @param ps_method An estimation model for the propensity score
#' (default: `SL.xgboost`).
#'
#' @return
#' A vector of propensity score estimates.
#'
#' @import SuperLearner
#'
#' @keywords internal
estimate_ps <- function(z, X, ps_method = "SL.xgboost") {
  sl_pscore <- SuperLearner::SuperLearner(Y = z, X = as.data.frame(X),
                                          newX = as.data.frame(X),
                                          family = binomial(),
                                          SL.library = ps_method,
                                          cvControl = list(V = 0))
  if (sum(sl_pscore$coef) == 0) sl_pscore$coef[1] <- 1
  est_ps <- as.numeric(sl_pscore$SL.predict)
  return(est_ps)
}
