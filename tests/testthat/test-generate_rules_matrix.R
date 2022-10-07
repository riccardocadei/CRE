test_that("Rules Extracted Correctly", {
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
  oreg_method <- NA
  ntrees_rf <- 100
  ntrees_gbm <- 50
  node_size <- 20
  max_nodes <- 5
  max_depth <- 15
  max_decay <- 0.025
  type_decay <- 2

  # Check for binary outcome
  binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)

  # Step 1: Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)

  ###### Discovery ######

  # Step 2: Estimate ITE
  ite_list <- estimate_ite(y, z, X, ite_method, binary,
                           include_ps = include_ps,
                           ps_method = ps_method,
                           oreg_method = oreg_method,
                           random_state = 376)
  ite <- ite_list[["ite"]]
  ite_std <- ite_list[["ite_std"]]

  # Step 3: Generate rules list
  initial_rules <- generate_rules(X, ite_std, ntrees_rf, ntrees_gbm, node_size,
                                  max_nodes, max_depth, random_state = 2389)

  rules <- prune_rules(initial_rules, X, ite_std, max_decay, type_decay)


  ###### Run Tests ######

  # Incorrect inputs
  expect_error(generate_rules_matrix(X = "test", rules))

  # Correct outputs
  rules_matrix <- generate_rules_matrix(X, rules)
  expect_identical(class(rules_matrix), c("matrix", "array"))
})
