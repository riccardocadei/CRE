test_that("Causal Rules Selected Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  ite_method <- "bart"
  include_ps <- "TRUE"
  ps_method <- "SL.xgboost"
  oreg_method <- NA
  ntrees_rf <- 100
  ntrees_gbm <- 50
  min_nodes <- 20
  max_nodes <- 5
  t <- 0.025
  q <- 0.8
  rules_method <- NA

  # Check for binary outcome
  binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)

  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)

  # Step 2: Estimate ITE
  ite_list <- estimate_ite(y, z, X, ite_method, binary,
                           include_ps = include_ps,
                           ps_method = ps_method,
                           oreg_method = oreg_method,
                           random_state = 328)
  ite <- ite_list[["ite"]]
  ite_std <- ite_list[["ite_std"]]

  # Step 3: Generate rules list
  initial_rules <- generate_rules(X, ite_std, ntrees_rf, ntrees_gbm, min_nodes,
                                  max_nodes, random_state = 2987)

  # Step 4: Generate rules matrix
  rules_all <- generate_rules_matrix(X, initial_rules, t)
  rules_matrix <- rules_all[["rules_matrix"]]
  rules_matrix_std <- rules_all[["rules_matrix_std"]]
  rules_list <- rules_all[["rules_list"]]

  ###### Run Tests ######

  # Incorrect inputs
  expect_error(select_causal_rules(rules_matrix_std = "test",
                                   rules_list, ite_std, binary, q,
                                   rules_method))
  expect_error(select_causal_rules(rules_matrix_std, rules_list,
                                   ite_std = "test", binary, q,
                                   rules_method))
  expect_error(select_causal_rules(rules_matrix_std, rules_list,
                                   ite_std, binary = "test", q,
                                   rules_method))

  # Correct outputs
  select_rules <- select_causal_rules(rules_matrix_std, rules_list, ite_std,
                                      binary, q, rules_method)
  expect_true(class(select_rules) == "character")
})
