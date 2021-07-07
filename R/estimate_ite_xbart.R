#' @title
#' Estimate the Individual Treatment Effect using Accelerated Bayesian Additive Regression Trees
#'
#' @description
#' Method for estimating the Individual Treatment Effect using Accelerated Bayesian Additive Regression Trees given a response vector, a treatment vector, and a features matrix
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
#' @examples
#' #TBD
#'
estimate_ite_xbart <- function(y, z, X, include_ps) {
  if (include_ps) {
    est_ps <- CRE::estimate_ps(z, X)
    X <- cbind(X, est_ps)
  }
  y_treated <- y[z==1]
  X_treated <- X[z==1,]
  y_control <- y[z==0]
  X_control <- X[z==0,]
  xbart_y1 <- XBART::XBART(y = as.matrix(y_treated), X = as.matrix(X_treated), Xtest = as.matrix(X),
                           num_trees = 30, num_sweeps = 40, max_depth = 250, Nmin = 1,
                           num_cutpoints = 50, alpha = 0.95, beta = 1.25, tau = var(y_treated) / 30,
                           no_split_penality = "Auto", burnin = 15, mtry = ncol(X),
                           p_categorical = ncol(X), kap = 1, s = 1, verbose = FALSE, parallel = TRUE)
  y1hat <- apply(xbart_y1$yhats_test[, 15:40], 1, mean)
  xbart_y0 <- XBART::XBART(y = as.matrix(y_control), X = as.matrix(X_control), Xtest = as.matrix(X),
                           num_trees = 30, num_sweeps = 40, max_depth = 250, Nmin = 1,
                           num_cutpoints = 50, alpha = 0.95, beta = 1.25, tau = var(y_control) / 30,
                           no_split_penality = "Auto",  burnin = 15, mtry = ncol(X),
                           p_categorical = ncol(X), kap = 1, s = 1, verbose = FALSE, parallel = TRUE)
  y0hat <- apply(xbart_y0$yhats_test[, 15:40], 1, mean)
  ite <- y1hat - y0hat
  return(ite)
}
