#' @title
#' Generate CRE Synthetic Data
#'
#' @description
#' Generates synthetic data with continues or binary outcome
#'
#' @param n An integer number that represents the number of observations. Non-integer
#' values will be converted into an integer number.
#' @param rho A positive double number (0,1) that represents the correlation
#' within the covariates (default: 0).
#' @param n_rules The number of causal rules, either 2 (default) or 4.
#' @param effect_size The effect size magnitude (default: 0.5).
#' TODO: what is the range of effect size magnitude?
#' @param p The number of covariates (default: 10).
#' @param binary Whether to use binary or continuous outcomes,
#'  either TRUE (default) or FALSE.
#'
#' @return
#' A list of synthetic data containing an outcome vector (y), a treatment
#' vector (z), and a matrix of p covariates (X).
#'
#' @examples
#' set.seed(123)
#' dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
#'                                 effect_size = 2, binary = FALSE)
#'
#' @export
#'
generate_cre_dataset <- function(n = 1000, rho = 0, n_rules = 2, p = 10,
                                 effect_size = 2, binary = FALSE) {

  # Check for correct binary input
  if (!(binary %in% c(TRUE, FALSE))) {
    stop("Invalid 'binary' input. Please specify TRUE or FALSE.")
  }

  if (is.numeric(n) & !is.integer(n)){
    n <- as.integer(n)
  }


  # Generate Variables
  mu <- rep(0, p)
  Sigma <- matrix(rho, nrow = p, ncol = p) + diag(p) * (1 - rho)
  rawvars <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
  pvars <- stats::pnorm(rawvars)
  X <- cbind(pvars[,1:5],
             stats::qbinom(pvars[,6:10], 1, 0.5))
  colnames(X) <- paste("x", 1:p, sep = "")
  X <- as.data.frame(X)

  # Generate Causal Rules and Treatment Effects
  if (binary == TRUE) {
    y0 <- rep(0, n)
    y0[X$x2 == 1 & X$x3 == 1] <- 1
    y1 <- rep(0, n)
    y1[X$x1 == 0 & X$x2 == 0] <- 1
    tau <- y1 - y0
  } else {
    stopifnot(n_rules %in% c(2, 4))
    if (n_rules == 2) {
      tau <- rep(0, n)
      tau[X$x1 == 0 & X$x2 == 0] = effect_size
      tau[X$x2 == 1 & X$x3 == 1] = - effect_size
    } else {
      tau <- rep(0, n)
      tau[X$x1 == 0 & X$x2 == 0] = effect_size
      tau[X$x1 == 1 & X$x2 == 1] = - effect_size
      tau[X$x2 == 0 & X$x3 == 0] = effect_size
      tau[X$x2 == 1 & X$x3 == 1] = - effect_size
    }
    y0 <- stats::rnorm(n, mean = X$x1 + 0.5 * X$x2 + X$x3, sd = 1)
    y1 <- y0 + tau
  }

  # Generate Treatment Variable
  logit.prob <- -1 + X$x1 - X$x2 + X$x3
  prob <- exp(logit.prob) / (1 + exp(logit.prob))

  # Generate Treatment Indicator
  z <- stats::rbinom(n, 1, prob = prob)

  # Generate Outcome
  y <- y0 * (1 - z) + y1 * z

  # Generate Observed Data
  dataset <- list(y = y, z = z, X = X)
  names(dataset) <- c("y", "z", "X")
  return(dataset)
}
