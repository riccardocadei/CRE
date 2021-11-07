#' @title
#' Estimate the Propensity Score
#'
#' @description
#' Method for estimating the Propensity Score given a treatment vector and features matrix
#'
#' @param z the treatment vector
#' @param X the features matrix
#'
#' @return a list of propensity score estimates
#'
#'
#' @export
#' @import SuperLearner
#'
#' @examples
#' dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2,
#'                                      effect_size = 2, binary = FALSE)
#'
#' # Initialize parameters
#' z <- dataset_cont[["z"]]
#' X <- as.data.frame(dataset_cont[["X"]])
#'
#' est_ps <- estimate_ps(z, X)
#'
estimate_ps <- function(z, X) {
  sl_pscore <- SuperLearner(Y = z, X = as.data.frame(X),
                                          newX = as.data.frame(X), family = binomial(),
                                          SL.library = "SL.xgboost", cvControl = list(V=0))
  est_ps <- as.numeric(sl_pscore$SL.predict)
  return(est_ps)
}
