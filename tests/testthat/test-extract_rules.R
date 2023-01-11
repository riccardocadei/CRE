test_that("Rules Extracted Correctly", {

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

  set.seed(349)
  seed_vector <- 1000 + sample.int(n = 10000000,
                                   size = ntrees + 2,
                                   replace = FALSE)
  random_state <- seed_vector[ntrees + 1]

  # Check for binary outcome
  binary_outcome <- ifelse(length(unique(y)) == 2, TRUE, FALSE)

  # Step 1: Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)

  # Step 2: Estimate ITE
  ite <- estimate_ite(y, z, X, ite_method,
                           binary_outcome = binary_outcome,
                           include_ps = include_ps,
                           ps_method = ps_method,
                           oreg_method = oreg_method,
                           random_state = random_state)

  expect_equal(ite[10], -1.240143, tolerance = 0.000001)
  expect_equal(ite[25], 0.8987101, tolerance = 0.000001)
  expect_equal(ite[70], 0.3728651, tolerance = 0.000001)


  # Set parameters
  N <- dim(X)[1]
  sf <- min(1, (11 * sqrt(N) + 1) / N)
  mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))



  # Random Forest
  set.seed(seed_vector[1])
  forest <- suppressWarnings(randomForest::randomForest(x = X, y = ite,
                                                        sampsize = sf * N,
                                                        replace = FALSE,
                                                        ntree = 1,
                                                        maxnodes = mn,
                                                        nodesize = node_size))
  for (i in 2:ntrees) {
    mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))
    set.seed(seed_vector[i])
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
  expect_equal(treelist[2]$list[[1]][2, 6], -0.3252457, tolerance = 0.000001)
  expect_equal(treelist[2]$list[[2]][3, 6], 0.8240253, tolerance = 0.000001)
  expect_equal(treelist[2]$list[[10]][3, 6], 0.8240253, tolerance = 0.000001)


  type_decay <- 2
  max_depth <- 15

  ###### Run Tests ######

  # Incorrect inputs
  expect_error(extract_rules(treelist = NA, X, ntrees, max_depth))
  expect_error(extract_rules(treelist, X = NA, ntrees, max_depth))
  expect_error(extract_rules(treelist, X, ntrees = -100, max_depth))
  #expect_error(extract_rules(treelist, X, ntrees, max_depth = -10))

  # Correct outputs
  rules_RF <- extract_rules(treelist, X, ntrees, max_depth)
  expect_true(any(class(rules_RF) == "matrix"))
  expect_equal(length(rules_RF), 438)
  expect_equal(rules_RF[3], "X[,5]>0.5 & X[,7]<=0.5")
})
