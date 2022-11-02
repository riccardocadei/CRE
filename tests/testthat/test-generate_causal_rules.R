test_that("generate_causal_rules works as expected!", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 300, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- as.data.frame(dataset_cont[["X"]])
  X_names <- names(as.data.frame(X))

  method_params = list(ratio_dis = 0.25,
                       ite_method_dis="bart",
                       ps_method_dis = "SL.xgboost",
                       oreg_method_dis = "SL.xgboost",
                       include_ps_dis = TRUE,
                       ite_method_inf = "bart",
                       ps_method_inf = "SL.xgboost",
                       oreg_method_inf = "SL.xgboost",
                       include_ps_inf = TRUE,
                       include_offset = FALSE,
                       cate_method = "DRLearner",
                       cate_SL_library = "SL.xgboost",
                       filter_cate = FALSE,
                       offset_name = NA,
                       random_state = 3591)

  hyper_params = list(intervention_vars = c(),
                      ntrees_rf = 100,
                      ntrees_gbm = 50,
                      node_size = 20,
                      max_nodes = 5,
                      max_depth = 15,
                      max_decay = 0.025,
                      type_decay = 2,
                      t_anom = 0.025,
                      t_corr = 1,
                      replace = FALSE,
                      stability_selection = TRUE,
                      cutoff = 0.8,
                      pfer = 0.1)

  # Input checks
  check_input_data(y = y, z = z, X = X)
  method_params <- check_method_params(y = y, params = method_params)
  check_hyper_params(params = hyper_params)

  # Estimate ITE
  ite_list <- estimate_ite(y = y, z = z, X = X,
                               ite_method = getElement(method_params,"ite_method_dis"),
                               is_y_binary = getElement(method_params,"is_y_binary"),
                               include_ps = getElement(method_params,"include_ps_dis"),
                               ps_method = getElement(method_params,"ps_method_dis"),
                               oreg_method = getElement(method_params,"oreg_method_dis"),
                               X_names = X_names,
                               include_offset = getElement(method_params,"include_offset"),
                               offset_name = getElement(method_params,"offset_name"),
                               random_state = getElement(method_params, "random_state"))
  ite <- ite_list[["ite"]]
  ite_std <- ite_list[["ite_std"]]

  # Generate Causal Decision Rules
  select_rules <- generate_causal_rules(X, ite_std, method_params, hyper_params)
  expect_true(class(select_rules[[1]]) == "character")

  hyper_params[["effect_modifiers"]] <- X_names[c(5,7,8,9)]
  select_rules <- generate_causal_rules(X, ite_std, method_params, hyper_params)
  expect_true(class(select_rules[[1]]) == "character")
})
