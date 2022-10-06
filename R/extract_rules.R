#' @title
#' Extract Causal Rules
#'
#' @description
#' Method for extracting causal rules from the Random Forest or Gradient
#' Boosting algorithms.
#'
#' @param treelist A list of decision trees.
#' @param X The features matrix.
#' @param ntrees The number of (the first) decision trees considered.
#' @param max_depth The number of top levels from each tree considered
#' to extract conditions.
#' @param ite_std The standardized ITE.
#' @param max_decay Decay Threshold for pruning the rules.
#' @param type_decay Decay Type for pruning the rules (1: relative error; 2: error).
#'
#' @keywords internal
#'
#' @return
#' A vector of causal rules.
#'
extract_rules <- function(treelist, X, ntrees, max_depth,
                          ite_std, max_decay, type_decay) {

  rules <- inTrees::extractRules(treeList = treelist,
                                X = X,
                                ntree = ntrees,
                                maxdepth = max_depth)
  rules <- c(rules)

  rules_matrix <- matrix(rules)
  colnames(rules_matrix) <- "condition"
  metric <- inTrees::getRuleMetric(rules_matrix,
                                  X,
                                  ite_std)

  # Prune irrelevant variable-value pair from a rule condition
  pruned <- inTrees::pruneRule(rules = metric,
                              X = X,
                              target = ite_std,
                              maxDecay = max_decay,
                              typeDecay = type_decay)
  return(unique(pruned[, 4]))
}
