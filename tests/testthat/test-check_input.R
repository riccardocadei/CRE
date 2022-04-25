test_that("check_input works as expected!", {


  dataset1 <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                  effect_size = 2, binary = FALSE)


  dataset2 <- generate_cre_dataset(n = 200, rho = 0, n_rules = 2, p = 10,
                                   effect_size = 2, binary = FALSE)


  params1 <- list(ratio_dis = 0.25,
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
                  ntrees_rf = 100,
                  ntrees_gbm = 50,
                  min_nodes = 20,
                  max_nodes = 5,
                  t = 0.025,
                  q = 0.8)


  # Different input data size
  expect_error(check_input(y = dataset1[["y"]],
                           z = dataset2[["z"]],
                           X = as.data.frame(dataset1[["X"]]),
                           params = params1))

  expect_error(check_input(y = dataset1[["y"]],
                           z = dataset1[["z"]],
                           X = as.data.frame(dataset2[["X"]]),
                           params = params1))

  expect_error(check_input(y = dataset2[["y"]],
                           z = dataset1[["z"]],
                           X = as.data.frame(dataset1[["X"]]),
                           params = params1))

  expect_error(check_input(y = "test",
                           z = dataset1[["z"]],
                           X = as.data.frame(dataset1[["X"]]),
                           params = params1))

  params2 <- list(ratio_dis = NA,
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
                  ntrees_rf = 100,
                  ntrees_gbm = 50,
                  min_nodes = 20,
                  max_nodes = 5,
                  t = 0.025,
                  q = 0.8)

  # Wrong ratio value for discovery
  expect_error(check_input(y = dataset1[["y"]],
                           z = dataset1[["z"]],
                           X = as.data.frame(dataset1[["X"]]),
                           params = params2))

  params2[["ratio_dis"]] <- 2
  expect_error(check_input(y = dataset1[["y"]],
                           z = dataset1[["z"]],
                           X = as.data.frame(dataset1[["X"]]),
                           params = params2))

  # incorrect ite method
  params3 <- list(ratio_dis = 0.25,
                  ite_method_dis = 0,
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
                  ntrees_rf = 100,
                  ntrees_gbm = 50,
                  min_nodes = 20,
                  max_nodes = 5,
                  t = 0.025,
                  q = 0.8)

  expect_error(check_input(y = dataset1[["y"]],
                           z = dataset1[["z"]],
                           X = as.data.frame(dataset1[["X"]]),
                           params = params3))

  params3[["ite_method_dis"]] <- "test"
  expect_error(check_input(y = dataset1[["y"]],
                           z = dataset1[["z"]],
                           X = as.data.frame(dataset1[["X"]]),
                           params = params3))

  params3[["ite_method_dis"]] <- "bart"
  params3[["ite_method_inf"]] <- 0
  expect_error(check_input(y = dataset1[["y"]],
                           z = dataset1[["z"]],
                           X = as.data.frame(dataset1[["X"]]),
                           params = params3))


  params3[["ite_method_inf"]] <- "test"
  expect_error(check_input(y = dataset1[["y"]],
                           z = dataset1[["z"]],
                           X = as.data.frame(dataset1[["X"]]),
                           params = params3))

  #TODO: Add check for other parameters.

})
