test_that("Extreme Rules Discarded Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5, binary_outcome = FALSE)
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
  replace <- FALSE
  max_decay <- 0.025
  type_decay <- 2
  t_ext <- 0.1
  intervention_vars <- c()

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

  # Step 3: Generate rules list
  initial_rules <- generate_rules(X, ite, intervention_vars, ntrees_rf,
                                  ntrees_gbm, node_size, max_nodes, max_depth,
                                  replace)

  rules_list <- filter_irrelevant_rules(initial_rules, X, ite, max_decay, type_decay)
  rules_matrix <- generate_rules_matrix(X, rules_list)

  ###### Run Tests ######

  # Incorrect inputs
  expect_error(filter_extreme_rules(rules_matrix = "test", rules_list, t_ext))

  # Correct outputs
  results <- filter_extreme_rules(rules_matrix, rules_list, t_ext)
  expect_identical(class(results), c("matrix", "array"))

  t_ext <- 0
  results <- filter_extreme_rules(rules_matrix, rules_list, t_ext)
  expect_identical(class(results), c("matrix", "array"))
})
