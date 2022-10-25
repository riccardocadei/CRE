#' @title
#' Extract (Causal) Decision Rules
#'
#' @description
#' Method for extracting causal rules from the Random Forest or Gradient
#' Boosting algorithms.
#'
#' @param treelist A list of decision trees.
#' @param X The features matrix.
#' @param ntrees The number of (the first) decision trees considered.
#' @param max_depth The number of top levels from each tree considered
#' @param digits The Number of digitits for rounding decision rules.
#' to extract conditions.
#'
#' @keywords internal
#'
#' @return
#' A vector of (Causal) Decision Rules.
#'
extract_rules <- function(treelist, X, ntrees, max_depth, digits=2) {

  rules <- inTrees::extractRules(treeList = treelist,
                                X = X,
                                ntree = ntrees,
                                maxdepth = max_depth,
                                digits = digits)
  return(rules)
}
