#' @title
#' Estimate the Individual Treatment Effect (ITE) using T-Learner.
#'
#' @description
#' Estimates the Individual Treatment Effect using T-Learner given a response
#' vector, a treatment vector, a features matrix and estimation model for the
#' outcome.
#'
#' @param y An observed response vector.
#' @param z A treatment vector.
#' @param X A features matrix.
#' @param learner_y A estimation model for the outcome.
#'
#' @return
#' A list of ITE estimates.
#'
#' @keywords internal
#'
estimate_ite_tlearner <- function(y, z, X, learner_y = "SL.xgboost") {

  logger::log_trace("learner_y: '{learner_y}' was selected.")

  X <- as.data.frame(X)
  y_0_model <- SuperLearner::SuperLearner(Y = y[z == 0],
                                          X = X[z == 0, ],
                                          family = gaussian(),
                                          SL.library = learner_y,
                                          cvControl = list(V = 0))
  if (sum(y_0_model$coef) == 0) y_0_model$coef[1] <- 1
  y_0_hat <- predict(y_0_model, X, onlySL = TRUE)$pred

  y_1_model <- SuperLearner::SuperLearner(Y = y[z == 1],
                                          X = X[z == 1, ],
                                          family = gaussian(),
                                          SL.library = learner_y,
                                          cvControl = list(V = 0))
  if (sum(y_1_model$coef) == 0) y_1_model$coef[1] <- 1
  y_1_hat <- predict(y_1_model, X, onlySL = TRUE)$pred

  ite <- as.vector(y_1_hat - y_0_hat)

  return(ite)
}
