#' @title
#' Generate rules
#'
#' @description
#' Generates a set of relevant decision rules characterizing the
#' heterogeneity in the ITE.
#'
#' @param X A covariate matrix.
#' @param ite A vector of estimated ITE.
#' @param intervention_vars A vector of intervention-able variables used for
#' rules generation.
#' @param ntrees_rf A number of decision trees for the random forest algorithm.
#' @param ntrees_gbm A number of decision trees for the gradient boosting
#' algorithm.
#' @param node_size A minimum size of the trees' terminal nodes.
#' @param max_nodes A maximum allowed number of terminal nodes per a tree in the
#' forest.
#' @param max_depth A number of top levels from each tree considered
#' to extract conditions.
#' @param replace A Boolean variable for replacement in bootstrapping.
#'
#' @return
#' A list of generated decision rules
#'
#' @keywords internal
#'
generate_rules <- function(X, ite, intervention_vars, ntrees_rf, ntrees_gbm,
                           node_size, max_nodes, max_depth, replace) {

  # Filter only Intervention-able variables ------------------------------------
  if (!is.null(intervention_vars)) X <- X[, intervention_vars, drop = FALSE]

  # Set parameters
  N <- dim(X)[1]
  sf <- min(1, (11 * sqrt(N) + 1) / N) # TODO: what is sf?
  mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))

  # Random Forest
  # TODO: replace splitting criteria enforcing heterogeneity
  forest <- randomForest::randomForest(x = X,
                                       y = ite,
                                       sampsize = sf * N,
                                       replace = replace,
                                       ntree = 1,
                                       maxnodes = mn,
                                       nodesize = node_size)
  for (i in 2:ntrees_rf) {
    mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))
    model1_RF <- randomForest::randomForest(x = X,
                                            y = ite,
                                            sampsize = sf * N,
                                            replace = replace,
                                            ntree = 1,
                                            maxnodes = mn,
                                            nodesize = node_size)

    forest <- randomForest::combine(forest, model1_RF)
  }

  treelist_RF <- inTrees::RF2List(forest)
  rules_RF <- extract_rules(treelist_RF, X, ntrees_rf, max_depth)

  # Gradient Boosting
  dist <- ifelse(is.numeric(ite), "gaussian", "bernoulli")

  if (is.numeric(ite) == FALSE) {
    ite <- as.numeric(ite) - 1
  }

  if (ntrees_gbm > 0) {
    model1_GB <- gbm::gbm.fit(x = X,
                              y = ite,
                              bag.fraction = sf,
                              n.trees = 1,
                              interaction.depth = (mn / 2),
                              shrinkage = 0.01,
                              distribution = dist,
                              verbose = FALSE,
                              n.minobsinnode = node_size)

    for (i in 2:ntrees_gbm) {
      model1_GB$interaction_depth <- (mn / 2)
      model1_GB <- gbm::gbm.more(model1_GB,
                                 n.new.trees = 1,
                                 verbose = FALSE)
    }

    treelist_GB <- inTrees::GBM2List(model1_GB, X)
    rules_GB <- extract_rules(treelist_GB, X, ntrees_gbm, max_depth)

    rules <- c(rules_RF, rules_GB)
  } else {
    rules <- rules_RF[, 1]
  }

  return(rules)
}
