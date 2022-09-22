test_that("cre Runs Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- as.data.frame(dataset_cont[["X"]])
  X_names <- names(as.data.frame(X))

  method_params = list(ratio_dis = 0.25,
                       ite_method_dis="bart",
                       include_ps_dis = TRUE,
                       ps_method_dis = "SL.xgboost",
                       ps_method_inf = "SL.xgboost",
                       ite_method_inf = "bart",
                       include_ps_inf = TRUE,
                       include_offset = FALSE,
                       cate_method = "DRLearner",
                       cate_SL_library = "SL.xgboost",
                       filter_cate = FALSE,
                       offset_name = NA,
                       random_state = 3591)

 hyper_params = list(ntrees_rf = 100,
                     ntrees_gbm = 50,
                     node_size = 20,
                     max_nodes = 5,
                     t = 0.025,
                     q = 0.8,
                     stability_selection = TRUE,
                     pfer_val = 0.1)


  #TODO: Need to move to a better place: Incorrect ntrees_rf, ntrees_gbm, node_size, max_nodes, t, q inputs
  hyper_params[["ntrees_rf"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["ntrees_rf"]] <- 100
  hyper_params[["ntrees_gbm"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["ntrees_gbm"]] <- 50
  hyper_params[["node_size"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["node_size"]] <- 5
  hyper_params[["max_nodes"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["max_nodes"]] <- 5
  hyper_params[["t"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["t"]] <- 0.025
  hyper_params[["q"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["q"]] <- 0.8
  hyper_params[["stability_selection"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  hyper_params[["stability_selection"]] <- TRUE
  hyper_params[["pfer_val"]] <- "test"
  expect_error(cre(y, z, X, method_params, hyper_params))

  # Correct outputs
  hyper_params[["pfer_val"]] <- 0.1
  cre_results <- cre(y, z, X, method_params, hyper_params)
  expect_true(class(cre_results) == "cre")
})
