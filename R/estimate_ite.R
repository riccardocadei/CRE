#' @title
#' Estimate the Individual Treatment Effect (ITE)
#'
#' @description
#' Estimates the Individual Treatment Effect given a response vector,
#' a treatment vector, a covariate matrix, and a desired algorithm.
#'
#' @param y The observed response vector.
#' @param z The observed treatment vector.
#' @param X The covariate matrix.
#' @param ite_method the method for estimating the Individual Treatment Effect.
#' Some methods requires additional parameters. These parameters are mentioned in
#' the indented blocks for each method and their definitions are provided at the
#' end of this parameters list.
#'   - `ipw`: Inverse Propensity Weighting
#'     - `ps_method`
#'   - `sipw`: Stabilized Inverse Propensity Weighting
#'     - `ps_method`
#'   - `aipw`: Augmented Inverse Propensity Weighting
#'     - `ps_method` and  `oreg_method`
#'   - `oreg`: Outcome Regression
#'   - `bart`: Bayesian Additive Regression Trees
#'     - `include_ps` and `ps_method`
#'   - `xbart`: Accelerated Bayesian Additive Regression Trees
#'     - `include_ps` and `ps_method`
#'   - `bcf`: Bayesian Causal Forest
#'     - `ps_method`
#'   - `xbcf`: Accelerated Bayesian Causal Forest
#'     - `ps_method`
#'   - `cf`: Causal Forest
#'     - `include_ps` and `ps_method`
#'   - `poisson`: Poisson Estimation
#'     - `X_names`, `include_offset`, `offset_name`
#' @param is_y_binary whether or not the outcome is binary
#' @param ... A dditional parameters passed to different models.
#' @details
#' ## Additional parameters
#'   - **include_ps**: Whether or not to include propensity score estimate as a
#'   covariate in ITE estimation.
#'   - **ps_method**: Estimation method for the propensity score. This include
#'   libraries for the SuperLearner package.
#'   - **oreg_method**: The estimation model for the outcome regressions. This
#'   include libraries for the SuperLearner package.
#'   - **X_names**: The names of the covariates. (TODO: Remove from input params.)
#'   - **include_offset**: Whether or not to include an offset when estimating
#'   the ITE.
#'   - **offset_name**: The name of the offset.
#'
#' @return
#' A list that includes:
#'   -  raw ITE estimates
#'   -  standardized ITE estimates, and
#'   -  standard deviations for the ITE estimates.
#'
#' @export
#'
estimate_ite <- function(y, z, X, ite_method, is_y_binary, ...){


  ## collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  check_args <- function(required_args, arg_names){
    for (arg in required_args){
      if (!is.element(arg,arg_names)){
        stop(paste('At least one argument is not provided. Missing argument: ',
                   arg, '.'))
      }
    }
  }

  if (ite_method == "ipw") {
    check_args(c('ps_method'), arg_names)
    ite <- estimate_ite_ipw(y, z, X, ps_method)
    sd_ite <- NA
  } else if (ite_method == "sipw") {
    check_args(c('ps_method'), arg_names)
    ite <- estimate_ite_sipw(y, z, X, ps_method)
    sd_ite <- NA
  } else if (ite_method == "aipw") {
    check_args(c('ps_method', 'oreg_method'), arg_names)
    ite <- estimate_ite_aipw(y, z, X, ps_method, oreg_method)
    sd_ite <- NA
  } else if (ite_method == "oreg") {
    ite <- estimate_ite_oreg(y, z, X)
    sd_ite <- NA
  } else if (ite_method == "bart") {
    check_args(c('include_ps', 'ps_method'), arg_names)
    ite_results <- estimate_ite_bart(y, z, X, include_ps, ps_method)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "xbart") {
    check_args(c('include_ps', 'ps_method'), arg_names)
    ite_results <- estimate_ite_xbart(y, z, X, include_ps, ps_method)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "bcf") {
    check_args(c('ps_method'), arg_names)
    ite_results <- estimate_ite_bcf(y, z, X, ps_method)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "xbcf") {
    check_args(c('ps_method'), arg_names)
    ite_results <- estimate_ite_xbcf(y, z, X, ps_method)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "cf") {
    check_args(c('include_ps', 'ps_method'), arg_names)
    ite_results <- estimate_ite_cf(y, z, X, include_ps, ps_method)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "poisson") {
    check_args(c('include_offset', 'offset_name', 'X_names'), arg_names)
    ite <- estimate_ite_poisson(y, z, X, X_names, include_offset, offset_name)
    sd_ite <- NA
  } else {
    stop(paste("Invalid ITE method. Please choose from the following:\n",
               "'ipw', 'sipw', 'aipw', 'oreg', 'bart', 'xbart', 'bcf', 'xbcf', ",
               "'cf', or 'poisson'"))
  }
  if (is_y_binary) {
    ite <- round(ite, 0)
  }
  ite_std <- (ite - mean(ite)) / stats::sd(ite)
  return(list(ite = as.vector(ite), ite_std = as.vector(ite_std),
              sd_ite = as.vector(sd_ite)))
}
