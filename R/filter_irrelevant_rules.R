#' @title
#' Filter irrelevant decision rules
#'
#' @description
#' Filter irrelevant decision rules extracted evaluating the performance
#' reduction removing a specific rule at the time
#' (see 'Interpreting tree ensembles with the inTrees package'
#' by Houtao Deng, 2019).
#'
#' @param rules A list of decision rules.
#' @param X A features matrix.
#' @param ite An estimated ITE.
#' @param max_decay A decay Threshold for pruning the rules.
#' @param type_decay A decay type for pruning the rules
#' (1: relative error; 2: error).
#'
#' @keywords internal
#'
#' @return
#' A list of the selected decision rules.
#'
filter_irrelevant_rules <- function(rules, X, ite, max_decay, type_decay) {

  rules_matrix <- matrix(rules)
  colnames(rules_matrix) <- "condition"
  metric <- inTrees::getRuleMetric(rules_matrix,
                                   X,
                                   ite)

  pruned <- inTrees::pruneRule(rules = metric,
                               X = X,
                               target = ite,
                               maxDecay = max_decay,
                               typeDecay = type_decay)
  rules <- unique(pruned[, 4])
  return(rules)
}
