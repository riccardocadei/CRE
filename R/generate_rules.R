#' @title
#' Generate Rules
#'
#' @description
#' Method for generating (Causal) Decision Rules before Regularization.
#'
#' @param X The covariate matrix.
#' @param ite_std The standardized ITE.
#' @param ntrees_rf The number of decision trees for randomForest.
#' @param ntrees_gbm The number of decision trees for gradient boosting.
#' @param node_size The minimum size of the trees' terminal nodes.
#' @param max_nodes The maximum number of terminal nodes trees in the forest can have.
#' @param max_depth The number of top levels from each tree considered
#' to extract conditions.
#' @param max_decay Decay Threshold for pruning the rules.
#' @param type_decay Decay Type for pruning the rules (1: relative error; 2: error).
#' @param random_state An integer number that represents a random state.
#'
#' @return
#' A vector of causal rules.
#'
#' @export
#'
generate_rules <- function(X, ite_std, ntrees_rf, ntrees_gbm, node_size, max_nodes,
                           max_depth, max_decay, type_decay, random_state) {

  # generate seed values
  seed_1 <- random_state + 1
  set.seed(random_state + 100)
  seed_2 <- sample(100000, ntrees_rf)
  set.seed(random_state + 1000)
  seed_3 <- sample(100000, 1)

  # Set parameters
  N <- dim(X)[1]
  sf <- min(1, (11 * sqrt(N) + 1) / N) # TODO: what is sf?
  mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))

  # Random Forest
  set.seed(seed_1)
  forest <- suppressWarnings(randomForest::randomForest(x = X, y = ite_std,
                                                        sampsize = sf * N,
                                                        replace = FALSE,
                                                        ntree = 1,
                                                        maxnodes = mn,
                                                        nodesize = node_size))
  for(i in 2:ntrees_rf) {
    mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))
    set.seed(seed_2[i])
    model1_RF <- suppressWarnings(
                  randomForest::randomForest(x = X,
                                             y = ite_std,
                                             sampsize = sf * N ,
                                             replace = FALSE,
                                             ntree = 1,
                                             maxnodes = mn,
                                             nodesize = node_size))

    forest <- randomForest::combine(forest, model1_RF)
  }

  treelist_RF <- inTrees::RF2List(forest)
  rules_RF <- extract_rules(treelist_RF, X, ntrees_rf, max_depth,
                            ite_std, max_decay, type_decay)

  # Gradient Boosting
  dist <- ifelse(is.numeric(ite_std), "gaussian", "bernoulli")

  if (is.numeric(ite_std) == FALSE) {
    ite_std <- as.numeric(ite_std) - 1
  }

  set.seed(seed_3)
  model1_GB <- gbm::gbm.fit(x = X, y = ite_std, bag.fraction = sf, n.trees = 1,
                            interaction.depth = (mn / 2), shrinkage = 0.01,
                            distribution = dist, verbose = FALSE,
                            n.minobsinnode = node_size)

  for(i in 2:ntrees_gbm) {
    model1_GB$interaction_depth <- (mn / 2)
    model1_GB <- gbm::gbm.more(model1_GB, n.new.trees = 1, verbose = FALSE)
  }


  treelist_GB <- inTrees::GBM2List(model1_GB, X)
  rules_GB <- extract_rules(treelist_GB, X, ntrees_gbm, max_depth,
                            ite_std, max_decay, type_decay)


  rules_list <- c(rules_RF, rules_GB)
  return(rules_list)
}
