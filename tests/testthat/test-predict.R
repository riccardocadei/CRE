test_that("predict function works as expected!", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 400, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary_outcome = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- as.data.frame(dataset_cont[["X"]])
  X_names <- names(as.data.frame(X))

  method_params <- list(ratio_dis = 0.5,
                        ite_method = "bart",
                        learner_ps = "SL.xgboost",
                        learner_y = "SL.xgboost")

  hyper_params <- list(intervention_vars = NULL,
                       offset = NULL,
                       ntrees_rf = 100,
                       ntrees_gbm = 50,
                       node_size = 20,
                       max_rules = 50,
                       max_depth = 3,
                       t_decay = 0.025,
                       t_ext = 0.025,
                       t_corr = 1,
                       stability_selection = "vanilla",
                       cutoff = 0.8,
                       pfer = 1,
                       B = 2,
                       subsample = 0.5)

  result <- cre(y, z, X, method_params, hyper_params)
  ite_pred <- predict(result, X)
  expect_true(length(ite_pred) == nrow(X))

  hyper_params$t_corr <- 0
  hyper_params$subsample <- 0.1
  result <- cre(y, z, X, method_params, hyper_params)
  expect_true(length(ite_pred) == nrow(X))
})
