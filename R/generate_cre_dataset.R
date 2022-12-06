#' @title
#' Generate CRE Synthetic Data
#'
#' @description
#' Generates synthetic data with continues or binary outcome
#'
#' @param n An integer number that represents the number of observations.
#' Non-integer values will be converted into an integer number.
#' @param rho A positive double number (0,1) that represents the correlation
#' within the covariates (default: 0).
#' @param n_rules The number of causal rules, either 2 (default) or 4.
#' @param effect_size The effect size magnitude (default: 2).
#' @param p The number of covariates (default: 10).
#' @param binary_covariates Whether to use binary or continuous covariates
#' (default: TRUE)
#' @param binary_outcome Whether to use binary or continuous outcomes
#' (default: TRUE)
#'
#' @return
#' A list of synthetic data containing an outcome vector (y), a treatment
#' vector (z), and a matrix of p covariates (X).
#'
#' @examples
#' set.seed(123)
#' dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
#'                                 effect_size = 2, binary_covariates = TRUE,
#'                                 binary_outcome = TRUE)
#'
#' @export
#'
generate_cre_dataset <- function(n = 1000, rho = 0, n_rules = 2, p = 10,
                                 effect_size = 2, binary_covariates = TRUE,
                                 binary_outcome = TRUE) {

  # Check for correct binary input
  if (!(binary_outcome %in% c(TRUE, FALSE))) {
    stop("Invalid 'binary' input. Please specify TRUE or FALSE.")
  }
  if (is.numeric(n) & !is.integer(n)){
    n <- as.integer(n)
  }

  # Generate Covariate Matrix
  mu <- rep(0, p)
  Sigma <- matrix(rho, nrow = p, ncol = p) + diag(p) * (1 - rho)
  rawvars <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
  pvars <- stats::pnorm(rawvars)
  if (binary_covariates) { X <- stats::qbinom(pvars, 1, 0.5) }
  else { X <- pvars }
  colnames(X) <- paste("x", 1:p, sep = "")
  X <- as.data.frame(X)

  # Generate Treatment Vector
  logit.prob <- -1 + X$x1 - X$x2 + X$x3
  prob <- exp(logit.prob) / (1 + exp(logit.prob))
  z <- stats::rbinom(n, 1, prob = prob)

  # Generate Causal Rules and Potential Outcomes
  stopifnot(n_rules %in% c(2, 4))
  if (binary_outcome == TRUE){
    y0 <- rep(0, n)
    y1 <- rep(0, n)
    effect_size = 1
  }
  else {
    y0 <- stats::rnorm(n, mean = 0, sd = 1)
    y1 <- y0
  }
  y0[X$x1 == 1 & X$x2 == 0] = effect_size
  y1[X$x5 == 1 & X$x6 == 0] = effect_size
  if (n_rules == 4) {
    y0[X$x4 == 1] = effect_size
    y1[X$x5 == 0 & X$x7 == 1 & X$x8 == 0] = effect_size
  }

  # Generate Outcome
  y <- y0 * (1 - z) + y1 * z

  # Compute Individual Treatment Effect
  ite <- y1 - y0

  # Generate Observed Data
  dataset <- list(y = y, z = z, X = X, ite = ite)
  return(dataset)
}
