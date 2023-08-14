test_that("Correlated Rules Discarded Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5,
                                       binary_outcome = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  ite_method <- "bart"
  learner_ps <- "SL.xgboost"
  learner_y <- NA
  ntrees <- 100
  node_size <- 20
  max_nodes <- 5
  max_depth <- 3
  t_decay <- 0.025
  t_corr <- 1
  t_ext <- 0.01
  intervention_vars <- c()

  # Check for binary outcome
  binary_outcome <- ifelse(length(unique(y)) == 2, TRUE, FALSE)

  # Step 1: Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)

  ###### Discovery ######

  # Step 2: Estimate ITE
  ite <- estimate_ite(y, z, X, ite_method,
                           learner_ps = learner_ps,
                           learner_y = learner_y)

  # Step 3: Generate rules list
  initial_rules <- generate_rules(X, ite, ntrees, node_size,
                                  max_nodes, max_depth)

  rules_list <- filter_irrelevant_rules(initial_rules, X,
                                        ite, t_decay)
  rules_matrix <- generate_rules_matrix(X, rules_list)

  rules_matrix <- filter_extreme_rules(rules_matrix, rules_list, t_ext)
  rules_list <- colnames(rules_matrix)

  ###### Run Tests ######

  # Incorrect inputs
  expect_error(filter_correlated_rules(rules_matrix = "test",
                                       rules_list,
                                       t_corr))

  # Correct outputs
  results <- filter_correlated_rules(rules_matrix, rules_list, t_corr)
  expect_identical(class(results), c("matrix", "array"))

  t_corr <- 10
  results <- filter_correlated_rules(rules_matrix, rules_list, t_corr)
  expect_identical(class(results), c("matrix", "array"))
})
