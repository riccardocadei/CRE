#' @title
#' Generate CRE synthetic data
#'
#' @description
#' Generates synthetic data sets to run simulation for causal inference
#' experiments composed by an outcome vector (`y`), a treatment vector (`z`),
#' a covariates matrix (`X`), and an unobserved individual treatment effects
#' vector (`ite`).
#' The arguments specify the data set characteristic, including the
#' number of individuals (`n`), the number of covariates (`p`), the correlation
#' within the covariates (`rho`),  the number of decision rules
#' (`n_rules`) decomposing the Conditional Average Treatment Effect (CATE), the
#' treatment effect magnitude (`effect_size`), the confounding mechanism
#' (`confounding`), and whether the covariates and outcomes are binary or
#' continuous (`binary_covariates`, `binary_outcome`).
#'
#' @details
#' The covariates matrix is generated with the specified correlation among
#' individuals, and each covariate is sampled either from a
#' \code{Bernoulli(0.5)} if binary, or a \code{Gaussian(0,1)} if continuous.
#' The treatment vector is sampled from a
#' \code{Bernoulli}(\eqn{\frac{1}{1+ \exp(1-x_1+x_2-x_3)}}), enforcing the treatment
#' assignment probabilities to be a function of observed covariates.
#' The potential outcomes (\eqn{y(0)} and \eqn{y(1)}) are then sampled from a Bernoulli
#' if binary, or a Gaussian (with standard deviation equal to 1) if continuous.
#' Their mean is equal to a confounding term (null, linear or non-linear and
#' always null for binary outcome) plus 1-4 decision rules weighted by the
#' treatment effect magnitude. The two potential outcomes characterizes the CATE
#' (and then the unobserved individual treatment effects vector) as the sum of
#' different additive contributions for each decision rules considered
#' (plus an intercept).
#' The final expression of the CATE depends on the treatment effect magnitude
#' and the number of decision rules considered.
#'
#' The 4 decision rules are:
#' - Rule 1: \eqn{1\{x_1 > 0.5; x_2 \leq 0.5\}(\textbf{x})}
#' - Rule 2: \eqn{1\{x_5 > 0.5; x_6 \leq 0.5\}(\textbf{x})}
#' - Rule 3: \eqn{1\{x_4 \leq 0.5\}(\textbf{x})}
#' - Rule 4: \eqn{1\{x_5 \leq 0.5; x_7 > 0.5; x_8 \leq 0.5\}(\textbf{x})}
#' with corresponding additive average treatment effect (AATE) equal to:
#' - Rule 1: \eqn{-} `effect_size`,
#' - Rule 2: \eqn{+} `effect_size`,
#' - Rule 3: \eqn{- 0.5 \cdot} `effect_size`,
#' - Rule 4: \eqn{+ 2 \cdot} `effect_size`.
#'
#' In example, setting `effect_size`=4 and `n_rules`=2:
#' \deqn{\text{CATE}(\textbf{x}) = -4 \cdot 1\{x_1 > 0.5; x_2 \leq 0.5\}(\textbf{x}) +
#' 4 \cdot 1\{x_5 > 0.5; x_6 \leq 0.5\}(\textbf{x})}
#'
#' The final outcome vector `y` is finally computed by combining the potential
#' outcomes according to the treatment assignment.
#'
#' @param n An integer number that represents the number of observations.
#' Non-integer values will be converted into an integer number.
#' @param rho A positive double number that represents the correlation
#' within the covariates (default: 0, range: [0,1)).
#' @param n_rules The number of causal rules (default: 2, range: {1,2,3,4}).
#' @param effect_size The treatment effect size magnitude (default: 2,
#' range: \eqn{\geq}0).
#' @param p The number of covariates (default: 10).
#' @param binary_covariates Whether to use binary or continuous covariates
#' (default: `TRUE`).
#' @param binary_outcome Whether to use binary or continuous outcomes
#' (default: `TRUE`).
#' @param confounding Only for continuous outcome, add confounding variables:
#' - `lin` for linear confounding,
#' - `nonlin` for non-linear confounding,
#' - `no` for no confounding (default).
#'
#' @return
#' A list, representing the generated synthetic data set, containing:
#'  \item{y}{an outcome vector,}
#'  \item{z}{a treatment vector,}
#'  \item{X}{a covariates matrix,}
#'  \item{ite}{an individual treatment vector.}
#'
#' @note
#' Set the covariates domain (`binary_covariates`) and outcome domain
#' (`binary_outcome`) according to the experiment of interest.
#' Increase complexity in heterogeneity discovery:
#' - decreasing the sample size (`n`),
#' - adding correlation among covariates (`rho`),
#' - increasing the number of rules (`n_rules`),
#' - increasing the number of covariates (`p`),
#' - decreasing the absolute value of the causal effect (`effect_size`),
#' - adding linear or not-linear confounders (`confounding`).
#'
#' @examples
#' set.seed(123)
#' dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
#'                                 effect_size = 2, binary_covariates = TRUE,
#'                                 binary_outcome = TRUE, confounding = "no")
#'
#' @export
#'
generate_cre_dataset <- function(n = 1000, rho = 0, n_rules = 2, p = 10,
                                 effect_size = 2, binary_covariates = TRUE,
                                 binary_outcome = TRUE, confounding = "no") {

  # Check for correct binary input
  if (!(binary_outcome %in% c(TRUE, FALSE))) {
    stop("Invalid 'binary' input. Please specify TRUE or FALSE.")
  }
  if (is.numeric(n) && !is.integer(n)) {
    n <- as.integer(n)
  }

  # Generate Covariate Matrix
  mu <- rep(0, p)
  Sigma <- matrix(rho, nrow = p, ncol = p) + diag(p) * (1 - rho)
  rawvars <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
  pvars <- stats::pnorm(rawvars)
  if (binary_covariates) {
    X <- stats::qbinom(pvars, 1, 0.5)
  } else {
    X <- pvars
  }
  colnames(X) <- paste("x", 1:p, sep = "")
  X <- as.data.frame(X)

  # Generate Treatment Vector
  logit_prob <- -1 + X$x1 - X$x2 + X$x3
  prob <- exp(logit_prob) / (1 + exp(logit_prob))
  z <- stats::rbinom(n, 1, prob = prob)

  # Generate Causal Rules and Potential Outcomes
  if (binary_outcome == TRUE) {
    y0 <- rep(0, n)
    y1 <- rep(0, n)
    effect_size <- 1
  } else {
    if (confounding == "lin") {
      mean <- X$x1 + X$x3 + X$x4
    } else if (confounding == "nonlin") {
      mean <- X$x1 + cos(X$x3*X$x4)
    } else if (confounding == "no") {
      mean <- 0
    } else {
      stop("Invalid 'confounding' input. Please input:
           'lin' for linear confounding,
           'nonlin' for non-linear confounding),
           'no' for no confounding")
    }
    y0 <- stats::rnorm(n, mean = mean, sd = 1)
    y1 <- y0
  }
  if (n_rules >= 1) {
    y0[X$x1 > 0.5 & X$x2 <= 0.5] <- y0[X$x1 > 0.5 & X$x2 <= 0.5] + effect_size
  }
  if (n_rules >= 2) {
    y1[X$x5 > 0.5 & X$x6 <= 0.5] <- y1[X$x5 > 0.5 & X$x6 <= 0.5] + effect_size
  }
  if (n_rules >= 3) {
    if (binary_outcome) {
      stop(paste("Synthtic dataset with binary outcome and ", n_rules,
                 "rules has not been implemented yet. ",
                 "Available 'n_rules' options: {1,2}."))
    }
    y0[X$x4 <= 0.5] <- y0[X$x4 <= 0.5] + 0.5 * effect_size
  }
  if (n_rules >= 4) {
    y1[X$x5 <= 0.5 & X$x7 > 0.5 & X$x8 <= 0.5] <-
                y1[X$x5 <= 0.5 & X$x7 > 0.5 & X$x8 <= 0.5] + 2 * effect_size
  }
  if (n_rules >= 5) {
    stop(paste("Synthtic dataset with continuos outcome and ", n_rules,
               "rules has not been implemented yet. ",
               "Available 'n_rules' options: {1,2,3,4}."))
  }


  # Generate Outcome
  y <- y0 * (1 - z) + y1 * z

  # Compute Individual Treatment Effect
  ite <- y1 - y0

  # Generate data set
  dataset <- list(y = y, z = z, X = X, ite = ite)
  return(dataset)
}
