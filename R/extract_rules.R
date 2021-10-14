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
#' @param take_1 whether or not to call the take1 helper function
#' @param type_decay the type of decay to apply when pruning the rules
#'
#' @return a vector of causal rules
#'
#' @export
#'
extract_rules <- function(treelist, X, ntrees, ite_std, take_1, type_decay) {
  rules <- inTrees::extractRules(treeList = treelist, X = X, ntree = ntrees, maxdepth = 15)
  rules <- c(rules)
  if (take_1) {
    rules <- rules[take1(length(rules))]
  }
  rules_matrix <- matrix(rules)
  colnames(rules_matrix) <- "condition"
  metric <- inTrees::getRuleMetric(rules_matrix, X, ite_std)
  pruned <- inTrees::pruneRule(metric, X, ite_std, 0.025, typeDecay = type_decay)
  return(unique(pruned[, 4]))
}
