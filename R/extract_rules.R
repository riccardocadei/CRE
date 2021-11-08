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
#' @examples
#'
#' dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2,
#' effect_size = 2, binary = FALSE)
#' y <- as.matrix(dataset_cont[["y"]])
#' z <- as.matrix(dataset_cont[["z"]])
#' X <- as.matrix(dataset_cont[["X"]])
#' ite_method <- "bcf"
#' include_ps <- "TRUE"
#' ntrees <- 100
#' min_nodes <- 20
#' max_nodes <- 5
#' binary <- FALSE
#'
#' # Estimate ITE
#' ite_list <- estimate_ite(y, z, X, ite_method, include_ps, binary)
#' ite <- ite_list[["ite"]]
#' ite_std <- ite_list[["ite_std"]]
#'
#' # Set parameters
#' N <- dim(X)[1]
#' sf <- min(1, (11 * sqrt(N) + 1) / N)
#' mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))
#' # Random Forest
#' forest <- suppressWarnings(randomForest::randomForest(x = X, y = ite_std,
#'                                                       sampsize = sf * N,
#'                                                    replace = FALSE,
#'                                                    ntree = 1, maxnodes = mn,
#'                                                    nodesize = min_nodes))
#' for(i in 2:ntrees) {
#'   mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))
#'   model1_RF <- suppressWarnings(randomForest::randomForest(x = X, y = ite_std,
#'                                                            sampsize = sf * N,
#'                                                            replace = FALSE,
#'                                                            ntree = 1, maxnodes = mn,
#'                                                            nodesize = min_nodes))
#'   forest <- randomForest::combine(forest, model1_RF)
#' }
#' treelist <- inTrees::RF2List(forest)
#' take_1 <- FALSE
#' type_decay <- 2
#'
#' rules_RF <- extract_rules(treelist, X, ntrees, ite_std, take_1, type_decay)
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
