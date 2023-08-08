test_that("Rules Extracted Correctly", {

  # Generate sample data
  set.seed(181)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary_outcome = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  ite_method <- "aipw"
  learner_ps <- "SL.xgboost"
  learner_y <- "SL.xgboost"
  ntrees <- 100
  node_size <- 20
  max_nodes <- 5

  # Check for binary outcome
  binary_outcome <- ifelse(length(unique(y)) == 2, TRUE, FALSE)

  # Step 1: Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)

  # Step 2: Estimate ITE
  ite <- estimate_ite(y, z, X, ite_method,
                           learner_ps = learner_ps,
                           learner_y = learner_y)

  expect_equal(ite[10], 0.6874263, tolerance = 0.000001)
  expect_equal(ite[25], -0.2175163, tolerance = 0.000001)
  expect_equal(ite[70], 1.656867, tolerance = 0.000001)


  # Set parameters
  N <- dim(X)[1]
  sf <- min(1, (11 * sqrt(N) + 1) / N)
  mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))

  # Random Forest
  forest <- suppressWarnings(randomForest::randomForest(x = X, y = ite,
                                                        sampsize = sf * N,
                                                        replace = FALSE,
                                                        ntree = 1,
                                                        maxnodes = mn,
                                                        nodesize = node_size))
  for (i in 2:ntrees) {
    mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))
    model1_RF <- suppressWarnings(randomForest::randomForest(
                                   x = X,
                                   y = ite,
                                   sampsize = sf * N,
                                   replace = FALSE,
                                   ntree = 1,
                                   maxnodes = mn,
                                   nodesize = node_size))
    forest <- randomForest::combine(forest, model1_RF)
  }
  treelist <- inTrees::RF2List(forest)

  expect_equal(length(treelist), 2)
  expect_equal(length(treelist[2]$list), 100)
  expect_equal(colnames(treelist[2]$list[[1]])[1], "left daughter")
  expect_equal(treelist[2]$list[[1]][2, 6], 0.4320062, tolerance = 0.000001)
  expect_equal(treelist[2]$list[[2]][3, 6], -0.5863133, tolerance = 0.000001)
  expect_equal(treelist[2]$list[[10]][3, 6], 0.06850307, tolerance = 0.000001)

  max_depth <- 15

  ###### Run Tests ######

  # Incorrect inputs
  expect_error(extract_rules(treelist = NA, X, ntrees, max_depth))
  expect_error(extract_rules(treelist, X = NA, ntrees, max_depth))
  expect_error(extract_rules(treelist, X, ntrees = -100, max_depth))

  # Correct outputs
  rules_RF <- extract_rules(treelist, X, ntrees, max_depth)
  expect_true(any(class(rules_RF) == "matrix"))
  expect_equal(length(rules_RF), 428)
  expect_equal(rules_RF[3], "X[,1]<=0.5 & X[,5]>0.5")
})
