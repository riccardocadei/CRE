#' @title
#' Estimate the Individual Treatment Effect using Bayesian Additive Regression Trees
#'
#' @description
#' Method for estimating the Individual Treatment Effect using Bayesian Additive Regression Trees given a response vector, a treatment vector, and a features matrix
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#'
#' @return a vector of ITE estimates
#'
#' @export
#'
#' @examples
#' #TBD
#'
estimate_ite_bart <- function(y, z, X) {
  y_treated <- y[z==1]
  X_treated <- X[z==1,]
  y_control <- y[z==0]
  X_control <- X[z==0,]
  bart_y1 <- wbart(x.train = X_treated, y.train = y_treated, x.test = X)
  y1hat <- bart_y1$yhat.test.mean
  bart_y0 <- wbart(x.train = X_control, y.train = y_control, x.test = X)
  y0hat <- bart_y0$yhat.test.mean
  ite <- y1hat - y0hat
  return(ite)
}
