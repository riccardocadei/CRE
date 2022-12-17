#' @title
#' Generate Rules
#'
#' @description
#' Method for generating a set of relevant Decision Rules characterizing the
#' heterogeneity in the ITE
#'
#' @param X The covariate matrix.
#' @param ite The estimated ITE.
#' @param intervention_vars Intervention-able variables used for Rules
#' Generation.
#' @param ntrees_rf The number of decision trees for randomForest.
#' @param ntrees_gbm The number of decision trees for gradient boosting.
#' @param node_size The minimum size of the trees' terminal nodes.
#' @param max_nodes The maximum number of terminal nodes trees in the forest can
#' have.
#' @param max_depth The number of top levels from each tree considered
#' to extract conditions.
#' @param replace Boolean variable for replacement in bootstrapping.
#'
#' @return
#' List of generated Decision Rules
#'
#' @keywords internal
#'
generate_rules <- function(X, ite, intervention_vars, ntrees_rf, ntrees_gbm,
                           node_size, max_nodes, max_depth, replace) {

  # Filter only Intervention-able variables ------------------------------------
  if (!is.null(intervention_vars)) X <- X[,intervention_vars,drop=FALSE]

  # Set parameters
  N <- dim(X)[1]
  sf <- min(1, (11 * sqrt(N) + 1) / N) # TODO: what is sf?
  mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))

  # Random Forest
  # TO DO: replace splitting criteria enforcing heterogeneity
  forest <- randomForest::randomForest(x = X,
                                       y = ite,
                                       sampsize = sf * N,
                                       replace = replace,
                                       ntree = 1,
                                       maxnodes = mn,
                                       nodesize = node_size)
  for(i in 2:ntrees_rf) {
    mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))
    model1_RF <- randomForest::randomForest(x = X,
                                            y = ite,
                                            sampsize = sf * N ,
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

  if (ntrees_gbm>0){
    model1_GB <- gbm::gbm.fit(x = X,
                              y = ite,
                              bag.fraction = sf,
                              n.trees = 1,
                              interaction.depth = (mn / 2),
                              shrinkage = 0.01,
                              distribution = dist,
                              verbose = FALSE,
                              n.minobsinnode = node_size)

    for(i in 2:ntrees_gbm) {
      model1_GB$interaction_depth <- (mn / 2)
      model1_GB <- gbm::gbm.more(model1_GB,
                                 n.new.trees = 1,
                                 verbose = FALSE)
    }

    treelist_GB <- inTrees::GBM2List(model1_GB, X)
    rules_GB <- extract_rules(treelist_GB, X, ntrees_gbm, max_depth)

    rules <- c(rules_RF, rules_GB)
  } else{
    rules <- rules_RF
  }

  return(rules)
}
