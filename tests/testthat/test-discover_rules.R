test_that("discover_rules works as expected!", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 300, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary_outcome = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- as.data.frame(dataset_cont[["X"]])
  X_names <- names(as.data.frame(X))

  method_params <- list(ratio_dis = 0.25,
                        ite_method_dis = "bart",
                        ps_method_dis = "SL.xgboost",
                        oreg_method_dis = "SL.xgboost",
                        include_ps_dis = TRUE,
                        ite_method_inf = "bart",
                        ps_method_inf = "SL.xgboost",
                        oreg_method_inf = "SL.xgboost",
                        include_ps_inf = TRUE,
                        offset = NA)

  hyper_params <- list(intervention_vars = c(),
                       ntrees_rf = 100,
                       ntrees_gbm = 50,
                       node_size = 20,
                       max_nodes = 5,
                       max_depth = 3,
                       t_decay = 0.025,
                       t_ext = 0.025,
                       t_corr = 1,
                       t_pvalue = 0.05,
                       replace = FALSE,
                       stability_selection = TRUE,
                       cutoff = 0.8,
                       pfer = 0.1,
                       penalty_rl = 1)

  # Input checks
  check_input_data(y = y, z = z, X = X)
  method_params <- check_method_params(y = y, X_names = names(X), ite = NULL,
                                       params = method_params)
  check_hyper_params(params = hyper_params)

  # Estimate ITE
  ite <- estimate_ite(y = y, z = z, X = X,
                ite_method = getElement(method_params, "ite_method_dis"),
                binary_outcome = getElement(method_params, "binary_outcome"),
                include_ps = getElement(method_params, "include_ps_dis"),
                ps_method = getElement(method_params, "ps_method_dis"),
                oreg_method = getElement(method_params, "oreg_method_dis"),
                X_names = X_names,
                offset = getElement(method_params, "offset"))

  # Generate Causal Decision Rules
  select_rules <- discover_rules(X, ite, method_params, hyper_params)
  expect_true(class(select_rules[[1]]) == "character")

  hyper_params[["effect_modifiers"]] <- X_names[c(5, 7, 8, 9)]
  select_rules <- discover_rules(X, ite, method_params, hyper_params)
  expect_true(class(select_rules[[1]]) == "character")
})
