#' @title
#' Estimate the Individual Treatment Effect using Augmented Inverse Propensity
#' Weighting
#'
#' @description
#' Estimates the Individual Treatment Effect using Augmented Inverse Propensity
#' Weighting given a response vector, a treatment vector, a features matrix,
#' an estimation model for the propensity score and estimation model for the
#' outcome regressions
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param ps_method the estimation model for the propensity score
#' @param oreg_method the estimation model for the outcome regressions
#'
#' @return
#' a list of ITE estimates and standard deviations for the ITE estimates
#'
#' @keywords internal
#'
#'
estimate_ite_aipw <- function(y, z, X, ps_method = "SL.xgboost",
                              oreg_method = "SL.xgboost") {

  phat <- estimate_ps(z, X, ps_method)

  sl_y <- SuperLearner(Y = y,
                       X = data.frame(X=X, Z=z),
                       family = gaussian(),
                       SL.library = oreg_method,
                       cvControl = list(V=0))

  pred_0 <- predict(sl_y, data.frame(X=X, Z=rep(0, nrow(X))), onlySL = T)
  pred_1 <- predict(sl_y, data.frame(X=X, Z=rep(1, nrow(X))), onlySL = T)

  apo_1 <- pred_1$pred + z*(y - pred_1$pred)/(phat)
  apo_0 <- pred_0$pred + (1 - z)*(y - pred_0$pred)/(1 - phat)

  ite <- as.vector(apo_1 - apo_0)

  return(ite)
}
