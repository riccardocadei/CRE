test_that("Rules Interpreted Correctly", {
  # Generate sample data
  set.seed(152)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary_outcome = FALSE)
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
  t_decay <- 0.025
  cutoff <- 0.8
  stability_selection <- TRUE
  pfer <- 0.1
  intervention_vars <- c()
  penalty_rl <- 1

  # Check for binary outcome
  binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)

  X_names <- names(as.data.frame(X))
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)

  # Step 2: Estimate ITE
  ite_list <- estimate_ite(y, z, X, ite_method, binary,
                           include_ps = include_ps,
                           ps_method = ps_method,
                           oreg_method = oreg_method,
                           random_state = 4568)
  ite <- ite_list[["ite"]]

  # Step 3: Generate rules list
  initial_rules <- generate_rules(X, ite, intervention_vars, ntrees_rf,
                                  ntrees_gbm, node_size, max_nodes, max_depth,
                                  replace)

  rules_list <- filter_irrelevant_rules(initial_rules, X, ite, t_decay)

  # Step 4: Generate rules matrix
  rules_matrix <- generate_rules_matrix(X, rules_list)
  rules_matrix_std <- standardize_rules_matrix(rules_matrix)

  # Step 5: Select Causal Rules
  select_rules <- as.character(select_rules(rules_matrix_std,
                                            rules_list,
                                            ite,
                                            cutoff,
                                            stability_selection,
                                            pfer,
                                            penalty_rl))

  ###### Run Tests ######

  # Incorrect inputs
  expect_warning(expect_error(interpret_rules(select_rules,
                                              X_names = NA)))

  # Correct outputs
  select_rules_interpretable <- interpret_rules(select_rules, X_names)
  expect_true(class(select_rules_interpretable) == "character")
  if (!is.na(select_rules_interpretable)) {
    expect_true(length(select_rules_interpretable) == length(select_rules))
  } else {
    expect_true(identical(select_rules, character(0)))
  }

})
