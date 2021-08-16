#' @title
#' Generate Rules
#'
#' @description
#' Method for generating causal rules
#'
#' @param X the features matrix
#' @param ite_std the standardized ITE
#' @param ntrees_rf the number of decision trees for randomForest
#' @param ntrees_gbm the number of decision trees for gradient boosting
#' @param min_nodes the minimum size of the trees' terminal nodes
#' @param max_nodes the maximum size of the trees' terminal nodes
#'
#' @return a vector of causal rules
#'
#' @export
#'
generate_rules <- function(X, ite_std, ntrees_rf, ntrees_gbm, min_nodes, max_nodes) {
  # Set parameters
  N <- dim(X)[1]
  sf <- min(1, (11 * sqrt(N) + 1) / N)
  mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))
  # Random Forest
  forest <- suppressWarnings(randomForest::randomForest(x = X, y = ite_std,
                                                        sampsize = sf * N,
                                                        replace = FALSE,
                                                        ntree = 1,
                                                        maxnodes = mn,
                                                        nodesize = min_nodes))
  for(i in 2:ntrees_rf) {
    mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))
    model1_RF <- suppressWarnings(randomForest::randomForest(x = X, y = ite_std,
                                                             sampsize = sf * N ,
                                                             replace = FALSE,
                                                             ntree = 1,
                                                             maxnodes = mn,
                                                             nodesize = min_nodes))
    forest <- randomForest::combine(forest, model1_RF)
  }
  treelist_RF <- inTrees::RF2List(forest)
  rules_RF <- extract_rules(treelist_RF, X, ntrees_rf, ite_std, FALSE, 2)
  # Gradient Boosting
  dist <- ifelse(is.numeric(ite_std), "gaussian", "bernoulli")
  if (is.numeric(ite_std) == FALSE) {
    ite_std <- as.numeric(ite_std) - 1
  }
  model1_GB <- gbm::gbm.fit(x = X, y = ite_std, bag.fraction = sf, n.trees = 1,
                            interaction.depth = (mn / 2), shrinkage = 0.01,
                            distribution = dist, verbose = FALSE,
                            n.minobsinnode = min_nodes)
  for(i in 2:ntrees_gbm) {
    model1_GB$interaction_depth <- (mn / 2)
    model1_GB <- gbm::gbm.more(model1_GB, n.new.trees = 1, verbose = FALSE)
  }
  treelist_GB <- inTrees::GBM2List(model1_GB, X)
  rules_GB <- extract_rules(treelist_GB, X, ntrees_gbm, ite_std, TRUE, 1)

  rules_list <- c(rules_RF, rules_GB)
  return(rules_list)
}
