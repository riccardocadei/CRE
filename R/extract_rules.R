#' @title
#' Extract Causal Rules
#'
#' @description
#' Method for extracting causal rules from the Random Forest or Gradient
#' Boosting algorithms.
#'
#' @param treelist A list of decision trees.
#' @param X The features matrix.
#' @param ntrees The number of decision trees.
#' @param ite_std The standardized ITE.
#' @param take_1 Whether or not to call the take1 helper function.
#' @param type_decay The type of decay to apply when pruning the rules.
#'
#' @keywords internal
#'
#' @return
#' A vector of causal rules.
#'
extract_rules <- function(treelist, X, ntrees, ite_std, take_1, type_decay) {

  rules <- inTrees_extractRules(treeList = treelist,
                                X = X,
                                ntree = ntrees,
                                maxdepth = 15)
  rules <- c(rules)

  if (take_1) {
    rules <- rules[take1(length(rules))]
  }

  rules_matrix <- matrix(rules)
  colnames(rules_matrix) <- "condition"
  metric <- inTrees_getRuleMetric(rules_matrix,
                                  X,
                                  ite_std)
  pruned <- inTrees_pruneRule(metric,
                              X,
                              ite_std,
                              0.025,
                              typeDecay = type_decay)
  return(unique(pruned[, 4]))
}
