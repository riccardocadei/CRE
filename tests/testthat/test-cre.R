test_that("cre Runs Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 400, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary_outcome = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- as.data.frame(dataset_cont[["X"]])
  ite <- dataset_cont[["ite"]]
  X_names <- names(as.data.frame(X))

  method_params <- list(ratio_dis = 0.25,
                        ite_method = "bart",
                        learner_ps = "SL.xgboost",
                        learner_y = "SL.xgboost")

 hyper_params <- list(intervention_vars = NULL,
                      offset = NULL,
                      ntrees = 50,
                      node_size = 10,
                      max_rules = 50,
                      max_depth = 3,
                      t_decay = 0.025,
                      t_ext = 0.025,
                      t_corr = 1,
                      stability_selection = "vanilla",
                      cutoff = 0.6,
                      pfer = 1,
                      B = 10,
                      subsample = 0.5)

  method_params[["ratio_dis"]] <- 2
  expect_error(cre(y, z, X, method_params, hyper_params))

  method_params[["ratio_dis"]] <- 0.25
  method_params[["ite_method"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  method_params[["ite_method"]] <- "aipw"
  method_params[["learner_ps"]] <- 1
  expect_error(cre(y, z, X, method_params, hyper_params))

  method_params[["learner_ps"]] <- "SL.xgboost"
  method_params[["learner_y"]] <- 1
  expect_error(cre(y, z, X, method_params, hyper_params))

  method_params[["learner_y"]] <- "SL.xgboost"
  method_params[["ite_method"]] <- "tpoisson"
  hyper_params[["offset"]] <- "test"
  expect_error(cre(y_temp, z, X, method_params, hyper_params))

  hyper_params[["offset"]] <- NULL
  hyper_params[["ntrees"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  method_params[["ite_method"]] <- "aipw"
  hyper_params[["ntrees"]] <- 0
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["ntrees"]] <- 40
  hyper_params[["node_size"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["node_size"]] <- 5
  hyper_params[["max_rules"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["max_rules"]] <- 5
  hyper_params[["t_ext"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["t_ext"]] <- 0.025
  hyper_params[["t_decay"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["t_decay"]] <- 0.025
  hyper_params[["t_corr"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["t_corr"]] <- 0.1
  hyper_params[["cutoff"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["cutoff"]] <- 0.8
  hyper_params[["stability_selection"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["stability_selection"]] <- "vanilla"
  hyper_params[["pfer"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["pfer"]] <- 1
  hyper_params[["B"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["B"]] <- 10
  hyper_params[["subsample"]] <- 2
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["subsample"]] <- 0.5
  hyper_params[["intervention_vars"]] <- c("test")
  expect_error(cre(y, z, X, method_params, hyper_params))

  # Correct outputs
  hyper_params[["intervention_vars"]] <- c("x1", "x2", "x5")
  cre_results <- cre(y, z, X, method_params, hyper_params)
  expect_true(class(cre_results) == "cre")

  hyper_params[["stability_selection"]] <- "error_control"
  cre_results <- cre(y, z, X, method_params, hyper_params)
  expect_true(class(cre_results) == "cre")

  method_params[["ite_method_dis"]] <- "aipw"
  method_params[["ite_method_inf"]] <- "aipw"
  cre_results <- cre(y, z, X, method_params, hyper_params)
  expect_true(class(cre_results) == "cre")

  cre_results <- cre(y, z, X, method_params, hyper_params, ite)
  expect_true(class(cre_results) == "cre")
})
