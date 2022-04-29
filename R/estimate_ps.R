#' @title
#' Estimate the Propensity Score
#'
#' @description
#' Method for estimating the Propensity Score given a treatment vector and features matrix
#'
#' @param z the treatment vector
#' @param X the features matrix
#' @param ps_method the estimation model for the propensity score (default: SL.xgboost).
#'
#' @return a list of propensity score estimates
#'
#' @import SuperLearner
#'
#' @examples
#' dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
#'                                 effect_size = 2, binary = FALSE)
#'
#' # Initialize parameters
#' z <- dataset[["z"]]
#' X <- as.data.frame(dataset[["X"]])
#' ps_method <- "SL.xgboost"
#'
#' est_ps <- CRE:::estimate_ps(z, X, ps_method)
#'
estimate_ps <- function(z, X, ps_method = "SL.xgboost") {
  sl_pscore <- SuperLearner::SuperLearner(Y = z, X = as.data.frame(X),
                                          newX = as.data.frame(X),
                                          family = binomial(),
                                          SL.library = ps_method,
                                          cvControl = list(V=0))
  est_ps <- as.numeric(sl_pscore$SL.predict)
  return(est_ps)
}
