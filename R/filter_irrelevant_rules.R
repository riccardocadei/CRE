#' @title
#' Filter irrelevant decision rules using leave-one-out pruning
#'
#' @description
#' Filters the irrelevant decision rules. The irrelevant rules are interpreted
#' as an error increase after removing a variable-value pair from the decision
#' rules (see "Interpreting tree ensembles with the inTrees package" by
#' Houtao Deng, 2019).
#'
#' @param rules A list of rules.
#' @param X A features matrix.
#' @param ite An estimated ITE.
#' @param t_decay The decay threshold for rules pruning.
#'
#' @keywords internal
#'
#' @return
#' A list of 'relevant' rules.
#'
filter_irrelevant_rules <- function(rules, X, ite, t_decay) {

  logger::log_debug("Filtering irrelevant rules...")
  ite_ <- ite - mean(ite)

  rules_matrix <- matrix(rules)
  colnames(rules_matrix) <- "condition"
  metric <- inTrees::getRuleMetric(rules_matrix,
                                   X,
                                   ite_)

  pruned <- inTrees::pruneRule(rules = metric,
                               X = X,
                               target = ite_,
                               maxDecay = t_decay)
  rules <- unique(pruned[, 4])

  logger::log_debug("Done with filtering irrelevant rules.")

  return(unique(rules))
}
