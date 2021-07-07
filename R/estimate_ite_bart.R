#' @title
#' Estimate the Individual Treatment Effect using Bayesian Additive Regression Trees
#'
#' @description
#' Method for estimating the Individual Treatment Effect using Bayesian Additive Regression Trees given a response vector, a treatment vector, and a features matrix
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param include_ps whether or not to include propensity score estimate as a covariate in ITE estimation
#'
#' @return a vector of ITE estimates
#'
#' @export
#'
estimate_ite_bart <- function(y, z, X, include_ps) {
  if (include_ps) {
    est_ps <- estimate_ps(z, X)
    X <- cbind(X, est_ps)
  }
  y_treated <- y[z==1]
  X_treated <- X[z==1,]
  y_control <- y[z==0]
  X_control <- X[z==0,]
  bart_y1 <- BART::wbart(x.train = X_treated, y.train = y_treated, x.test = X)
  y1hat <- bart_y1$yhat.test.mean
  bart_y0 <- BART::wbart(x.train = X_control, y.train = y_control, x.test = X)
  y0hat <- bart_y0$yhat.test.mean
  ite <- y1hat - y0hat
  return(ite)
}
