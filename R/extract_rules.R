#' @title
#' Extract Rules
#'
#' @description
#' Method for extracting causal rules from the Random Forest or Gradient Boosting algorithms
#'
#' @param treelist a list of decision trees
#' @param X the features matrix
#' @param ntrees the number of decision trees
#' @param ite_std the standardized ITE
#'
#' @return a vector of causal rules
#'
#' @export
#'
extract_rules <- function(treelist, X, ntrees, ite_std) {
  rules <- inTrees::extractRules(treeList = treelist, X = X, ntree = ntrees, maxdepth = 15)
  rules <- c(rules)
  rules <- rules[take1(length(rules))]
  rules_matrix <- matrix(rules)
  colnames(rules_matrix) <- "condition"
  metric <- inTrees::getRuleMetric(rules_matrix, X, ite_std)
  pruned <- inTrees::pruneRule(metric, X, ite_std, 0.025, typeDecay = 1)
  return(unique(pruned[, 4]))
}
