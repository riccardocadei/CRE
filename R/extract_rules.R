#' @title
#' Extract (causal) decision rules
#'
#' @description
#' Extracts causal rules from the random forest or the gradient
#' boosting algorithms.
#'
#' @param treelist A list of decision trees.
#' @param X Features matrix.
#' @param ntrees A number of (the first) decision trees considered.
#' @param max_depth A number of top levels from each tree considered.
#' @param digits A Number of digits for rounding decision rules to extract
#' conditions.
#'
#' @keywords internal
#'
#' @return
#' A vector of (causal) decision rules.
#'
extract_rules <- function(treelist, X, ntrees, max_depth, digits=2) {

  rules <- inTrees::extractRules(treeList = treelist,
                                 X = X,
                                 ntree = ntrees,
                                 maxdepth = max_depth,
                                 digits = digits)
  return(rules)
}
