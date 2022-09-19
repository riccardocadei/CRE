test_that("generate_rules works as expected!", {
  # Generate sample data
  set.seed(3784)
  dataset_cont <- generate_cre_dataset(n = 300, rho = 0, n_rules = 2, p = 10,
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
  ite_list <- estimate_ite(y, z, X, ite_method, binary,
                           include_ps = include_ps,
                           ps_method = ps_method,
                           or_method = or_method,
                           random_state = 298)
  ite <- ite_list[["ite"]]
  ite_std <- ite_list[["ite_std"]]


  # Correct outputs
  initial_rules <- generate_rules(X,
                                  ite_std,
                                  ntrees_rf,
                                  ntrees_gbm,
                                  min_nodes,
                                  max_nodes,
                                  random_state = 215)
  expect_true(class(initial_rules) == "character")

  # Activate the following test after addressing reproduciblity issue.
  expect_equal(length(initial_rules), 123)
  expect_equal(initial_rules[23], "X[,2]>0.5 & X[,9]<=0.5")
  expect_equal(initial_rules[107], "X[,2]<=0.5 & X[,4]<=0.5")
})
