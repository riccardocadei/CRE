#' @title
#' Generate rules
#'
#' @description
#' Generates a list of rules characterizing the heterogeneity in the Conditional
#' Average Treatment Effect (CATE) by tree-based methods (i.e., random forest).
#'
#' @param X A covariate matrix.
#' @param ite A vector of estimated ITE.
#' @param ntrees The number of decision trees for the random forest algorithm.
#' @param node_size Minimum size of the trees' terminal nodes.
#' @param max_rules Maximum number of candidate decision rules.
#' @param max_depth Maximum rules length.
#'
#' @return
#' A list of rules (names).
#'
#' @keywords internal
#'
generate_rules <- function(X, ite, ntrees, node_size, max_rules, max_depth) {

  logger::log_debug("Generating (candidate) rules...")
  st_time <- proc.time()

  # TODO: replace splitting criteria enforcing heterogeneity
  if (ntrees > 0) {
    N <- dim(X)[1]
    sampsize <- 0.5 * N
    forest <- randomForest::randomForest(x = X,
                                         y = ite,
                                         sampsize = sampsize,
                                         ntree = ntrees,
                                         maxnodes = 2^max_depth,
                                         nodesize = node_size)

    treelist <- inTrees::RF2List(forest)
    rules <- extract_rules(treelist, X, max_depth)
  } else {
    rules <- NULL
  }

  rule_counts <- table(unlist(rules))
  M <- min(max_rules, length(rule_counts))
  rules <- names(sort(rule_counts, decreasing = TRUE)[1:M])

  en_time <- proc.time()
  logger::log_debug("Done with generating (candidate) rules.. ",
                    "(WC: {g_wc_str(st_time, en_time)}", ".)")
  return(rules)
}
