#' @title
#' Estimate the Individual Treatment Effect (ITE) using Augmented Inverse
#' Probability Weighting (AIPW)
#'
#' @description
#' Estimates the Individual Treatment Effect using Augmented Inverse Probability
#' Weighting given a response vector, a treatment vector, a features matrix,
#' an estimation model for the propensity score and estimation model for the
#' outcome.
#'
#' @param y An observed response vector.
#' @param z A treatment vector.
#' @param X A features matrix.
#' @param learner_ps A estimation model for the propensity score.
#' @param learner_y A estimation model for the outcome.
#'
#' @return
#' A list of ITE estimates.
#'
#' @keywords internal
#'
estimate_ite_aipw <- function(y, z, X, learner_ps = "SL.xgboost",
                              learner_y = "SL.xgboost") {

  logger::log_trace("learner_ps: '{learner_ps}' and learner_y: '{learner_y}'",
                    " were provided.")

  ps_hat <- estimate_ps(z, X, learner_ps)

  y_model <- SuperLearner::SuperLearner(Y = y,
                                        X = data.frame(X = X, Z = z),
                                        family = gaussian(),
                                        SL.library = learner_y,
                                        cvControl = list(V = 0))

  if (sum(y_model$coef) == 0) y_model$coef[1] <- 1

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
