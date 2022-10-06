test_that("Rules Pruned Correctly", {

  # Generate sample data
  set.seed(181)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary = FALSE)
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
  max_depth <- 15

  set.seed(349)
  seed_vector <- 1000 + sample.int(n = 10000000,
                                   size = ntrees+2,
                                   replace = FALSE)
  random_state <- seed_vector[ntrees+1]

  # Check for binary outcome
  binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)

  # Step 1: Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)

  # Step 2: Estimate ITE
  ite_list <- estimate_ite(y, z, X, ite_method, binary,
                           include_ps = include_ps,
                           ps_method = ps_method,
                           oreg_method = oreg_method,
                           random_state = random_state)
  ite <- ite_list[["ite"]]
  ite_std <- ite_list[["ite_std"]]
  #sd_ite <- ite_list[["sd_ite"]]

  expect_equal(ite[10], -5.101268327, tolerance = 0.000001)
  expect_equal(ite[25], -0.9622934128, tolerance = 0.000001)
  expect_equal(ite[70], 7.98951025, tolerance = 0.000001)

  expect_equal(ite_std[15], 0.8319789286 , tolerance = 0.000001)
  expect_equal(ite_std[44], -0.4318524937, tolerance = 0.000001)
  expect_equal(ite_std[82], -0.1296018405, tolerance = 0.000001)


  # expect_equal(sd_ite[9], 1.152046, tolerance = 0.000001)
  # expect_equal(sd_ite[51], 1.136721, tolerance = 0.000001)
  # expect_equal(sd_ite[93], 1.139932, tolerance = 0.000001)


  # Set parameters
  N <- dim(X)[1]
  sf <- min(1, (11 * sqrt(N) + 1) / N)
  mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))



  # Random Forest
  set.seed(seed_vector[1])
  forest <- suppressWarnings(randomForest::randomForest(x = X, y = ite_std,
                                                        sampsize = sf * N,
                                                        replace = FALSE,
                                                        ntree = 1, maxnodes = mn,
                                                        nodesize = node_size))
  for(i in 2:ntrees) {
    mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))
    set.seed(seed_vector[i])
    model1_RF <- suppressWarnings(randomForest::randomForest(x = X, y = ite_std,
                                                             sampsize = sf * N,
                                                             replace = FALSE,
                                                             ntree = 1, maxnodes = mn,
                                                             nodesize = node_size))
    forest <- randomForest::combine(forest, model1_RF)
  }
  treelist <- inTrees::RF2List(forest)

  expect_equal(length(treelist),2)
  expect_equal(length(treelist[2]$list),100)
  expect_equal(colnames(treelist[2]$list[[1]])[1], "left daughter")
  expect_equal(treelist[2]$list[[1]][2,6], -0.1950480515, tolerance = 0.000001)
  expect_equal(treelist[2]$list[[2]][3,6], -0.3603212639, tolerance = 0.000001)
  expect_equal(treelist[2]$list[[10]][3,6], -0.1344911363, tolerance = 0.000001)

  rules <- extract_rules(treelist, X, ntrees, max_depth)

  max_decay <- 0.025
  type_decay <- 2

  ###### Run Tests ######

  # Incorrect inputs
  expect_error(prune_rules(rules = NA, X, ite_std, max_decay, type_decay))
  expect_error(prune_rules(rules, X = NA, ite_std, max_decay, type_decay))
  expect_error(prune_rules(rules, X, ite_std = NA, max_decay, type_decay))
  expect_error(prune_rules(rules, X, ite_std, max_decay = NA, type_decay))
  expect_error(prune_rules(rules, X, ite_std, max_decay, type_decay = NA))


  # Correct outputs
  rules_RF <- prune_rules(rules, X, ite_std, max_decay, type_decay)
  expect_true(class(rules_RF) == "character")
  #expect_equal(length(rules_RF), 6)
  #expect_equal(rules_RF[3], "X[,3]>0.5 & X[,10]<=0.5")
})
