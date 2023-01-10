test_that("Filter ireelevant rules run correctly", {

  # Generate sample data
  set.seed(181)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary_outcome = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  ite_method <- "ipw"
  include_ps <- "TRUE"
  ps_method <- "SL.xgboost"
  oreg_method <- NA
  ntrees <- 100
  node_size <- 20
  max_nodes <- 5
  max_depth <- 3

  # Step 1: Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)

  # Step 2: Estimate ITE
  ite_list <- estimate_ite(y, z, X, ite_method,
                           include_ps = include_ps,
                           ps_method = ps_method,
                           oreg_method = oreg_method)
  ite <- ite_list[["ite"]]

  expect_equal(ite[10], -1.240143, tolerance = 0.000001)
  expect_equal(ite[25], 0.8987101, tolerance = 0.000001)
  expect_equal(ite[70], 0.3728651, tolerance = 0.000001)


  # Set parameters
  N <- dim(X)[1]
  sf <- min(1, (11 * sqrt(N) + 1) / N)
  mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))

  # Random Forest
  forest <- suppressWarnings(randomForest::randomForest(
                              x = X,
                              y = ite,
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
  expect_equal(treelist[2]$list[[1]][2, 6], 0.23361, tolerance = 0.000001)
  expect_equal(treelist[2]$list[[2]][3, 6], 0.4554385, tolerance = 0.000001)
  expect_equal(treelist[2]$list[[10]][3, 6], 0.2910133, tolerance = 0.000001)

  rules <- extract_rules(treelist, X, ntrees, max_depth)

  t_decay <- 0.025

  ###### Run Tests ######

  # Incorrect inputs
  expect_error(filter_irrelevant_rules(rules = NA, X, ite, t_decay))
  expect_error(filter_irrelevant_rules(rules, X = NA, ite, t_decay))
  expect_error(filter_irrelevant_rules(rules, X, ite = NA, t_decay))
  expect_error(filter_irrelevant_rules(rules, X, ite, t_decay = NA))

  # Correct outputs
  rules_RF <- filter_irrelevant_rules(rules, X, ite, t_decay)
  expect_true(class(rules_RF) == "character")
})
