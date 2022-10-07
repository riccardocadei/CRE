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
  node_size <- 20
  max_nodes <- 5
  max_depth = 15
  max_decay = 0.025
  type_decay = 2
  t <- 0.025
  q <- 0.8
  stability_selection <- TRUE
  pfer_val <- 0.1

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
  initial_rules <- generate_rules(X, ite_std, ntrees_rf, ntrees_gbm, node_size,
                                  max_nodes, max_depth, random_state = 2987)

  rules <- prune_rules(initial_rules, X, ite_std, max_decay, type_decay)


  # Step 4: Generate rules matrix
  rules_all <- generate_rules_matrix(X, rules, t)
  rules_matrix <- rules_all[["rules_matrix"]]
  rules_matrix_std <- standardize_rules_matrix(rules_matrix)
  rules_list <- rules_all[["rules_list"]]

  ###### Run Tests ######

  # Incorrect inputs
  expect_error(select_causal_rules(rules_matrix_std = "test",
                                   rules_list, ite_std, q,
                                   stability_selection, pfer_val))
  expect_error(select_causal_rules(rules_matrix_std, rules_list,
                                   ite_std = "test", q,
                                   stability_selection, pfer_val))
  expect_error(select_causal_rules(rules_matrix_std, rules_list,
                                   ite_std, q = "test",
                                   stability_selection, pfer_val))
  expect_error(select_causal_rules(rules_matrix_std, rules_list,
                                   ite_std, q, stability_selection = "test",
                                   pfer_val))
  expect_error(select_causal_rules(rules_matrix_std, rules_list,
                                   ite_std, q, stability_selection,
                                   pfer_val = "test"))

  # Correct outputs
  select_rules <- select_causal_rules(rules_matrix_std, rules_list, ite_std,
                                    q, stability_selection, pfer_val)
  expect_true(class(select_rules) == "character")

  stability_selection <- FALSE
  select_rules <- select_causal_rules(rules_matrix_std, rules_list, ite_std,
                                      q, stability_selection, pfer_val)
  expect_true(class(select_rules) == "character")
})
