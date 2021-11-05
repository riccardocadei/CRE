#' @title
#' Generate CRE Synthetic Data
#'
#' @description
#' Generates synthetic data with continues or binary outcome for testing the
#'  Causal Rule Ensemble package
#'
#' @param n the number of observations
#' @param rho the correlation within the covariates (default: 0)
#' @param n_rules the number of causal rules, either 2 (default) or 4
#' @param effect_size the effect size magnitude (default: 0.5)
#' TODO: what is the range of effect size magnitude?
#' @param binary whether to use binary or continuous outcomes,
#'  either TRUE (default) or FALSE
#'
#' @return
#' a list of synthetic data containing an outcome vector, a treatment vector,
#'  and a matrix of 10 covariates
#'
#' @examples
#' set.seed(123)
#' dataset_c <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2,
#' effect_size = 0.5, binary = FALSE)
#'
#' @export
#'
generate_cre_dataset <- function(n, rho = 0, n_rules = 2, p = 10,
                                 effect_size = 0.5, binary = FALSE) {

  # Check for correct binary input
  if (!(binary %in% c(TRUE, FALSE))) {
    stop("Invalid 'binary' input. Please specify TRUE or FALSE.")
  }

  # Generate Variables
  mu <- rep(0, p)
  Sigma <- matrix(rho, nrow = p, ncol = p) + diag(p) * (1 - rho)
  rawvars <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
  pvars <- stats::pnorm(rawvars)
  X <- stats::qbinom(pvars, 1, 0.5)
  colnames(X) <- paste("x", 1:p, sep = "")

  # Generate Causal Rules and Treatment Effects
  if (binary) {
    y0 <- rep(0, n)
    y0[x2 == 1 & x3 == 1] <- 1
    y1 <- rep(0, n)
    y1[x1 == 0 & x2 == 0] <- 1
    tau <- y1 - y0
  } else {
    stopifnot(n_rules %in% c(2, 4))
    if (n_rules == 2) {
      tau <- rep(0, n)
      tau[x1 == 0 & x2 == 0] = effect_size
      tau[x2 == 1 & x3 == 1] = - effect_size
    } else {
      tau <- rep(0, n)
      tau[x1 == 0 & x2 == 0] = effect_size
      tau[x1 == 1 & x2 == 1] = - effect_size
      tau[x2 == 0 & x3 == 0] = effect_size
      tau[x2 == 1 & x3 == 1] = - effect_size
    }
    y0 <- stats::rnorm(n, mean = x1 + 0.5 * x2 + x3, sd = 1)
    y1 <- y0 + tau
  }

  # Generate Treatment Variable
  logit.prob <- -1 + x1 - x2 + x3
  prob <- exp(logit.prob) / (1 + exp(logit.prob))

  # Generate Treatment Indicator
  z <- stats::rbinom(n, 1, prob = prob)

  # Generate Outcome
  y <- y0 * (1 - z) + y1 * z

  # Observed Data
  dataset <- list(y = y, z = z, X = X)
  names(dataset) <- c("y", "z", "X")
  return(dataset)
}
