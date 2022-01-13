test_that("Rules Generated Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  ite_method <- "bart"
  include_ps <- "TRUE"
  ps_method <- "SL.xgboost"
  or_method <- NA
  ntrees_rf <- 100
  ntrees_gbm <- 50
  min_nodes <- 20
  max_nodes <- 5

  # Check for binary outcome
  binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)

  # Step 1: Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)

  # Step 2: Estimate ITE
  ite_list <- estimate_ite(y, z, X, ite_method, include_ps, ps_method,
                           or_method, binary)
  ite <- ite_list[["ite"]]
  ite_std <- ite_list[["ite_std"]]

  ###### Run Tests ######

  # Incorrect inputs
  expect_error(generate_rules(X = "test", ite_std, ntrees_rf, ntrees_gbm, min_nodes, max_nodes))
  expect_error(generate_rules(X, ite_std = "test", ntrees_rf, ntrees_gbm, min_nodes, max_nodes))
  expect_error(generate_rules(X, ite_std, ntrees_rf = "test", ntrees_gbm, min_nodes, max_nodes))
  expect_error(generate_rules(X, ite_std, ntrees_rf, ntrees_gbm = "test", min_nodes, max_nodes))
  expect_error(generate_rules(X, ite_std, ntrees_rf, ntrees_gbm, min_nodes = "test", max_nodes))
  expect_error(generate_rules(X, ite_std, ntrees_rf, ntrees_gbm, min_nodes, max_nodes = "test"))

  # Correct outputs
  initial_rules <- generate_rules(X, ite_std, ntrees_rf, ntrees_gbm, min_nodes, max_nodes)
  expect_true(class(initial_rules) == "character")
})
