#' @title
#' Filter irrelevant decision rules
#'
#' @description
#' Filter irrelevant decision rules extracted evaluating the performance
#' reduction removing a specific rule at the time
#' (see 'Interpreting tree ensembles with the inTrees package'
#' by Houtao Deng, 2019).
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

  rules_matrix <- matrix(rules)
  colnames(rules_matrix) <- "condition"
  metric <- inTrees::getRuleMetric(rules_matrix,
                                   X,
                                   ite)

  pruned <- inTrees::pruneRule(rules = metric,
                               X = X,
                               target = ite,
                               maxDecay = t_decay)
  rules <- unique(pruned[, 4])

  for (i in 1:length(rules)) {
    if (!grepl("&", rules[i]) & grepl("<=", rules[i])) {
      rules[i] <- sub("<=", ">", rules[i])
    }
  }

  logger::log_debug("Done with filtering irrelevant rules.")

  return(unique(rules))
}
