#' @title
#' Estimate the Individual Treatment Effect (ITE) using X-Learner.
#'
#' @description
#' Estimates the Individual Treatment Effect using X-Learner given a response
#' vector, a treatment vector, a features matrix and an estimation model for
#' the outcome.
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
estimate_ite_xlearner <- function(y, z, X, learner_y = "SL.xgboost") {

  logger::log_trace("learner_y: '{learner_y}' was selected.")

  X <- as.data.frame(X)

  y_0_model <- SuperLearner::SuperLearner(Y = y[z == 0],
                                          X = X[z == 0, ],
                                          family = gaussian(),
                                          SL.library = learner_y,
                                          cvControl = list(V = 0))

  if (sum(y_0_model$coef) == 0) y_0_model$coef[1] <- 1

  y_1_model <- SuperLearner::SuperLearner(Y = y[z == 1],
                                          X = X[z == 1, ],
                                          family = gaussian(),
                                          SL.library = learner_y,
                                          cvControl = list(V = 0))

  if (sum(y_1_model$coef) == 0) y_1_model$coef[1] <- 1

  ite <- array(0, dim = length(y))
  ite[z == 0] <- predict(y_1_model, X[z == 0, ], onlySL = TRUE)$pred - y[z == 0]
  ite[z == 1] <- y[z == 1] - predict(y_0_model, X[z == 1, ], onlySL = TRUE)$pred
  ite <- as.vector(ite)

  return(ite)
}
