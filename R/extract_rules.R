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
#' @param type_decay The type of decay to apply when pruning the rules.
#' (1: relative error; 2: error)
#'
#' @keywords internal
#'
#' @return
#' A vector of causal rules.
#'
extract_rules <- function(treelist, X, ntrees, ite_std, type_decay) {

  rules <- inTrees::extractRules(treeList = treelist,
                                X = X,
                                ntree = ntrees,
                                maxdepth = 15)
  rules <- c(rules)

  rules_matrix <- matrix(rules)
  colnames(rules_matrix) <- "condition"
  metric <- inTrees::getRuleMetric(rules_matrix,
                                  X,
                                  ite_std)
  pruned <- inTrees::pruneRule(metric,
                              X,
                              ite_std,
                              0.025,
                              typeDecay = type_decay)
  return(unique(pruned[, 4]))
}
