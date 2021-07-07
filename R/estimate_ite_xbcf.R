#' @title
#' Estimate the Individual Treatment Effect using Accelerated Bayesian Causal Forest
#'
#' @description
#' Method for estimating the Individual Treatment Effect using Accelerated Bayesian Causal Forest given a response vector, a treatment vector, and a features matrix
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
#' TBD
#'
estimate_ite_xbcf <- function(y, z, X) {
  est_ps <- estimate_ps(z, X)
  xbcf_model <- XBCF::XBCF(y = as.matrix(y), z = as.matrix(z), x_con = as.matrix(X),
                           x_mod = as.matrix(X), pihat = as.matrix(est_ps), num_sweeps = 40,
                           burnin = 15, max_depth = 250, num_cutpoints = 20,
                           mtry_con = ncol(X), mtry_mod = ncol(X),
                           pcat_con = ncol(X), pcat_mod = ncol(X),
                           n_trees_con = 30, tau_con = 10 * var(y) / 30,
                           n_trees_mod = 10, tau_mod = 1 * var(y) / 10)
  th_xbcf <- xbcf_model$tauhats * sd(y)
  b_xbcf <- xbcf_model$b_draws
  for (i in 16:40) {
    th_xbcf[, i] <- th_xbcf[, i] * (b_xbcf[i, 2] - b_xbcf[i, 1])
  }
  ite <- rowMeans(th_xbcf[, 16:40])
  return(ite)
}
