#' @title
#' Estimate the Individual Treatment Effect (ITE)
#'
#' @description
#' Estimates the Individual Treatment Effect given a response vector,
#' a treatment vector, a covariate matrix, and a desired algorithm.
#'
#' @param y An observed response vector.
#' @param z An observed treatment vector.
#' @param X A covariate matrix.
#' @param ite_method A method for estimating the Individual Treatment Effect.
#' Some methods requires additional parameters. These parameters are mentioned
#' in the indented blocks for each method and their definitions are provided at
#' the end of this parameters list.
#'   - `ipw`: Inverse Propensity Weighting.
#'     - `ps_method`
#'   - `sipw`: Stabilized Inverse Propensity Weighting.
#'     - `ps_method`
#'   - `aipw`: Augmented Inverse Propensity Weighting.
#'     - `ps_method` and  `oreg_method`
#'   - `oreg`: Outcome Regression.
#'   - `bart`: Bayesian Additive Regression Trees.
#'     - `include_ps` and `ps_method`
#'   - `bcf`: Bayesian Causal Forest.
#'     - `ps_method`
#'   - `cf`: Causal Forest.
#'     - `include_ps` and `ps_method`
#'   - `poisson`: Poisson Estimation.
#'     - `X_names`, `offset`
#' @param ... Additional parameters passed to different models.
#' @details
#' ## Additional parameters
#'   - **include_ps**: Whether or not to include propensity score estimate as a
#'   covariate in ITE estimation.
#'   - **ps_method**: An estimation method for the propensity score. This
#'   includes libraries for the SuperLearner package.
#'   - **oreg_method**: An estimation model for the outcome regressions. This
#'   includes libraries for the SuperLearner package.
#'   - **X_names**: The names of the covariates.
#'   (TODO: Remove from input params.)
#'   - **offset**: Name of the covariate to use as offset (i.e. 'x1') for
#'     Poisson ITE Estimation. `NULL` if offset is not used.
#'
#' @return
#' A list of ITE estimates.
#'
#' @keywords internal
#'
estimate_ite <- function(y, z, X, ite_method, ...) {


  # Address visible binding error.
  X_names <- offset <- oreg_method <- NULL
  include_ps <- ps_method <- ps_method_dis <- ps_method_inf <- NULL



  ## collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i, unlist(dot_args[i], use.names = FALSE))
  }

  check_args <- function(required_args, arg_names) {
    for (arg in required_args){
      if (!is.element(arg, arg_names)) {
        stop(paste("At least one argument is not provided. Missing argument: ",
                   arg, "."))
      }
    }
  }

  if (ite_method == "ipw") {
    check_args(c('ps_method'), arg_names)
    ite <- estimate_ite_ipw(y, z, X, ps_method)
  } else if (ite_method == "sipw") {
    check_args(c("ps_method"), arg_names)
    ite <- estimate_ite_sipw(y, z, X, ps_method)
  } else if (ite_method == "aipw") {
    check_args(c("ps_method", "oreg_method"), arg_names)
    ite <- estimate_ite_aipw(y, z, X, ps_method, oreg_method)
  } else if (ite_method == "oreg") {
    ite <- estimate_ite_oreg(y, z, X)
  } else if (ite_method == "bart") {
    check_args(c("include_ps", "ps_method"), arg_names)
    ite <- estimate_ite_bart(y, z, X, include_ps, ps_method)
  } else if (ite_method == "bcf") {
    check_args(c('ps_method'), arg_names)
    ite <- estimate_ite_bcf(y, z, X, ps_method)
  } else if (ite_method == "cf") {
    check_args(c("include_ps", "ps_method"), arg_names)
    ite <- estimate_ite_cf(y, z, X, include_ps, ps_method)
  } else if (ite_method == "poisson") {
    check_args(c("offset", "X_names"), arg_names)
    ite <- estimate_ite_poisson(y, z, X, X_names, offset)
  } else {
    stop(paste("Invalid ITE method. Please choose from the following:\n",
               "'ipw', 'sipw', 'aipw', 'oreg', 'bart', 'bcf', 'cf'",
               " or 'poisson'"))
  }

  binary_outcome <- ifelse(length(unique(y)) == 2, TRUE, FALSE)
  if (binary_outcome) {
    negative <- ite < -0.5
    ite[negative] <- -1
    positive <- ite > 0.5
    ite[positive] <- +1
    zero <- ite>=-0.5 & ite<=0.5
    ite[zero] <- 0
  }

  return(ite)
}
