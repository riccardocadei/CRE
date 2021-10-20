#' @title
#' Estimate the Individual Treatment Effect using Outcome Regression
#'
#' @description
#' Method for estimating the Individual Treatment Effect using Outcome Regression given a response vector, a treatment vector, and a features matrix
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#'
#' @return a vector of ITE estimates
#'
#' @export
#'
estimate_ite_or <- function(y, z, X) {
  y_treated <- y[z==1]
  X_treated <- X[z==1,]
  y_control <- y[z==0]
  X_control <- X[z==0,]
  temp1 <- BART::wbart(x.train = X_treated, y.train = y_treated, x.test = X_control)
  y1hat_control <- temp1$yhat.test.mean
  temp0 <- BART::wbart(x.train = X_control, y.train = y_control, x.test = X_treated)
  y0hat_treated <- temp0$yhat.test.mean
  ite <- rep(NA, times = length(y))
  ite[z==0] <- y1hat_control - y[z==0]
  ite[z==1] <- y[z==1] - y0hat_treated
  return(ite)
}
