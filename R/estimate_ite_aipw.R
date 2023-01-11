#' @title
#' Estimate the Individual Treatment Effect (ITE) using Augmented Inverse
#' Probability Weighting (AIPW)
#'
#' @description
#' Estimates the Individual Treatment Effect using Augmented Inverse Probability
#' Weighting given a response vector, a treatment vector, a features matrix,
#' an estimation model for the propensity score and estimation model for the
#' outcome regressions.
#'
#' @param y An observed response vector.
#' @param z A treatment vector.
#' @param X A features matrix.
#' @param ps_method A estimation model for the propensity score.
#' @param oreg_method A estimation model for the outcome regressions.
#'
#' @return
#' A list of ITE estimates.
#'
#' @keywords internal
#'
estimate_ite_aipw <- function(y, z, X, ps_method = "SL.xgboost",
                              oreg_method = "SL.xgboost") {

  ps_hat <- estimate_ps(z, X, ps_method)

  y_model <- SuperLearner(Y = y,
                          X = data.frame(X = X, Z = z),
                          family = gaussian(),
                          SL.library = oreg_method,
                          cvControl = list(V=0))

  y_0_hat <- predict(y_model,
                    data.frame(X = X, Z = rep(0, nrow(X))),
                    onlySL = TRUE)$pred
  y_1_hat <- predict(y_model,
                     data.frame(X = X, Z = rep(1, nrow(X))),
                     onlySL = TRUE)$pred

  apo_1 <- y_1_hat + z * (y - y_1_hat) / (ps_hat)
  apo_0 <- y_0_hat + (1 - z) * (y - y_0_hat) / (1 - ps_hat)

  ite <- as.vector(apo_1 - apo_0)

  return(ite)
}
