test_that("CRE Runs Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- as.data.frame(dataset_cont[["X"]])
  X_names <- names(as.data.frame(X))

  params <- list( ratio_dis = 0.25,
                  ite_method_dis = "bart",
                  include_ps_dis = "TRUE",
                  ps_method_dis = "SL.xgboost",
                  or_method_dis = NA,
                  ite_method_inf = "bart",
                  include_ps_inf = "TRUE",
                  ps_method_inf = "SL.xgboost",
                  or_method_inf = NA,
                  ntrees_rf = 100,
                  ntrees_gbm = 50,
                  min_nodes = 20,
                  max_nodes = 5,
                  t = 0.025,
                  q = 0.8,
                  rules_method = NA,
                  include_offset = FALSE,
                  offset_name = NA,
                  cate_method = "DRLearner",
                  cate_SL_library = "SL.xgboost",
                  filter_cate = FALSE)




  #TODO: Need to move to a better place: Incorrect ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q inputs
  params[["ntrees_rf"]] <- "test"
  expect_error(cre(y, z, X, params))

  params[["ntrees_rf"]] <- 100
  params[["ntrees_gbm"]] <- "test"
  expect_error(cre(y, z, X, params))

  params[["ntrees_gbm"]] <- 50
  params[["min_nodes"]] <- "test"
  expect_error(cre(y, z, X, params))

  params[["min_nodes"]] <- 5
  params[["max_nodes"]] <- "test"
  expect_error(cre(y, z, X, params))

  params[["max_nodes"]] <- 5
  params[["t"]] <- "test"
  expect_error(cre(y, z, X, params))

  params[["t"]] <- 0.025
  params[["q"]] <- "test"
  expect_error(cre(y, z, X, params))


  # Correct outputs
  params[["q"]] <- 0.8
  cre_results <- cre(y, z, X, params)
  expect_true(class(cre_results) == "cre")
})
