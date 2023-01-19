#' @title
#' Estimate the Individual Treatment Effect (ITE) using S-Learner.
#'
#' @description
#' Estimates the Individual Treatment Effect using S-Learner given a response
#' vector, a treatment vector, a features matrix and estimation model for the
#' outcome regressions.
#'
#' @param y An observed response vector.
#' @param z A treatment vector.
#' @param X A features matrix.
#' @param oreg_method An estimation model for the outcome regressions.
#'
#' @return
#' A list of ITE estimates.
#'
#' @keywords internal
#'
estimate_ite_slearner <- function(y, z, X, oreg_method = "SL.xgboost") {

  logger::log_trace("oreg_method: '{oreg_method}' was selected.")

  y_model <- SuperLearner::SuperLearner(Y = y,
                                        X = data.frame(X = X, Z = z),
                                        family = gaussian(),
                                        SL.library = oreg_method,
                                        cvControl = list(V = 0))

  if (sum(y_model$coef) == 0) y_model$coef[1] <- 1

  y_0_hat <- predict(y_model,
                     data.frame(X = X, Z = rep(0, nrow(X))),
                     onlySL = TRUE)$pred

  y_1_hat <- predict(y_model,
                     data.frame(X = X, Z = rep(1, nrow(X))),
                     onlySL = TRUE)$pred

  ite <- as.vector(y_1_hat - y_0_hat)

  return(ite)
}
