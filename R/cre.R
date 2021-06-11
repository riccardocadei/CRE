#' @title
#' The Causal Rule Ensemble
#'
#' @description
#' Method for performing the Causal Rule Ensemble on a dataset with a response variable, a treatment variable, and various features
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param ratio_dis the ratio of data delegated to the discovery subsample
#' @param ite_method_dis the method to estimate the discovery sample ITE
#' @param ite_method_inf the method to estimate the inference sample ITE
#' @param ntrees the number of decision trees
#' @param min_nodes the minimum size of the trees' terminal nodes
#' @param max_nodes the maximum size of the trees' terminal nodes
#' @param rules_method the method for performing penalized regression on the causal rules to select only those that are important
#' @param t the common support used in generating the causal rules matrix
#'
#' @export
#'
#' @return a list containing a select list of causal rules, Conditional Average Treatment Effect estimates, and a sensitivity analysis
#'
cre <- function(y, z, X, ratio_dis, ite_method_dis, ite_method_inf, ntrees, min_nodes,
                max_nodes, rules_method, t){
  #TBD
}
