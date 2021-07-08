#' @title
#' Generate CRE Synthetic Data
#'
#' @description
#' Method for generating synthetic data for testing the Causal Rule Ensemble package
#'
#' @param n the number of observations
#' @param rho the correlation within the variables
#' @param n_rules the number of causal rules, either 2 or 4
#' @param effect_size the effect size magnitude
#' @param binary whether to use binary or continuous outcomes, either TRUE or FALSE
#' @param seed the seed to set when generating data
#'
#' @return a list of synthetic data containing an outcome vector, a treatment vector, and a matrix of 10 covariates
#'
#' @export
#'
generate_cre_dataset <- function(n, rho, n_rules, effect_size, binary, seed) {
  # Check for correct binary input
  if (!(binary %in% c(TRUE, FALSE))) {
    stop("Invalid 'binary' input. Please specify TRUE or FALSE.")
  }

  # Set seed
  set.seed(seed)

  # Generate Variables
  p <- 10
  mu <- rep(0, p)
  Sigma <- matrix(rho, nrow = p, ncol = p) + diag(p) * (1 - rho)
  rawvars <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
  pvars <- stats::pnorm(rawvars)
  X <- stats::qbinom(pvars, 1, 0.5)
  colnames(X) <- paste("X", 1:10, sep = "")
  x1 <- X[,1]
  x2 <- X[,2]
  x3 <- X[,3]
  x4 <- X[,4]
  x5 <- X[,5]
  x6 <- X[,6]
  x7 <- X[,7]
  x8 <- X[,8]
  x9 <- X[,9]
  x10 <- X[,10]

  # Generate Causal Rules
  stopifnot(n_rules %in% c(2, 4))
  if (n_rules == 2) {
    tau = rep(0, n)
    tau[x1==0 & x2==0] = effect_size
    tau[x2==1 & x3==1] = -effect_size
  } else {
    tau = rep(0, n)
    tau[x1==0 & x2==0] = effect_size
    tau[x1==1 & x2==1] = -effect_size
    tau[x2==0 & x3==0] = effect_size
    tau[x2==1 & x3==1] = -effect_size
  }

  # Generate Treatment Effects
  if (binary) {
    y0 <- stats::rbinom(n, 1, 0.5)
    y1 <- y0 + tau
  } else {
    y0 <- stats::rnorm(n, mean = x1 + 0.5 * x2 + x3, sd = 1)
    y1 <- y0 + tau
  }

  # Generate Treatment Variable
  logit.prob <- -1 + x1 - x2 + x3
  prob <- exp(logit.prob) / (1 + exp(logit.prob))

  # Generate Treatment Indicator
  z <- stats::rbinom(n, 1, prob = prob)

  # Generate Outcome
  if (binary) {
    y <- y0 * (1 - z) + y1 * z
  } else {
    y <- y0 * (1 - z) + y1 * z + x1 - x2 + x3
  }

  # Observed Data
  dataset <- list(y = y, z = z, X = X)
  return(dataset)
}
