#' @title
#' Estimate the Individual Treatment Effect
#'
#' @description
#' Estimates the Individual Treatment Effect given a response vector,
#' a treatment vector, a features matrix, and a desired algorithm.
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param ite_method the method for estimating the Individual Treatment Effect:
#'   - `ipw`: Inverse Propensity Weighting
#'   - `sipw`: Stabilized Inverse Propensity Weighting
#'   - `or`: Outcome Regression, TODO: change this into a non reserved term.
#'   - `bart`: BART
#'   - `xbart`: Accelerated BART
#'   - `bcf`: Bayesian Causal Forest
#'   - `xbcf`: Accelerated Bayesian Causal Forest
#'   - `cf`: Causal Forest
#' @param include_ps whether or not to include propensity score estimate as a
#' covariate in ITE estimation
#' @param method_ps estimation method for the propensity score
#' @param binary whether or not the outcome is binary
#' @param X_names the names of the covariates
#' @param include_offset whether or not to include an offset when estimating
#' the ITE, for poisson only
#' @param offset_name the name of the offset, if it is to be included
#'
#' @return
#' a list that includes:
#'   -  raw ITE estimates
#'   -  standardized ITE estimates, and
#'   -  standard deviations for the ITE estimates.
#'
#' @export
#'
#' @examples
#' dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
#'                                      effect_size = 2, binary = FALSE)
#'
#' # Initialize parameters
#' y <- dataset[["y"]]
#' z <- dataset[["z"]]
#' X <- as.data.frame(dataset_cont[["X"]])
#' ite_method <- "bcf"
#' include_ps <- TRUE
#' binary <- FALSE
#' X_names <- names(as.data.frame(X))
#' include_offset <- FALSE
#' offset_name <- NA
#' method_ps <- "SL.xgboost"
#'
#' ite_list <- estimate_ite(y, z, X, ite_method, include_ps, method_ps, binary,
#'                          X_names, include_offset, offset_name)
#'
estimate_ite <- function(y, z, X, ite_method, include_ps, method_ps, binary, X_names,
                         include_offset, offset_name) {

  if (ite_method == "ipw") {
    ite <- estimate_ite_ipw(y, z, X, method_ps)
    sd_ite <- NA
  } else if (ite_method == "sipw") {
    ite <- estimate_ite_sipw(y, z, X, method_ps)
    sd_ite <- NA
  } else if (ite_method == "or") {
    ite <- estimate_ite_or(y, z, X)
    sd_ite <- NA
  } else if (ite_method == "bart") {
    ite_results <- estimate_ite_bart(y, z, X, include_ps, method_ps)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "xbart") {
    ite_results <- estimate_ite_xbart(y, z, X, include_ps, method_ps)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "bcf") {
    ite_results <- estimate_ite_bcf(y, z, X, method_ps)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "xbcf") {
    ite_results <- estimate_ite_xbcf(y, z, X, method_ps)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "cf") {
    ite_results <- estimate_ite_cf(y, z, X, include_ps, method_ps)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "poisson") {
    ite_results <- estimate_ite_poisson(y, z, X, X_names,
                                        include_offset, offset_name)
    ite <- ite_results[[1]]
    sd_ite <- NA
  } else {
    stop(paste("Invalid ITE method. Please choose from the following:\n",
               "'ipw', 'sipw', 'or', 'bart', 'xbart', 'bcf', 'xbcf', 'cf', ",
               " or 'poisson'"))
  }
  if (binary) {
    ite <- round(ite, 0)
  }
  ite_std <- (ite - mean(ite)) / stats::sd(ite)
  return(list(ite = ite, ite_std = ite_std, sd_ite = sd_ite))
}
