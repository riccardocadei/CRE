test_that("CRE Runs Correctly", {
  # Generate sample data
  dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2,
                                       effect_size = 2, binary = FALSE, seed = 2021)
  y <- abs(dataset_cont[["y"]])
  z <- dataset_cont[["z"]]
  X <- as.data.frame(dataset_cont[["X"]])
  X_names <- names(as.data.frame(X))
  ratio_dis <- 0.25
  ite_method_dis <- "bcf"
  include_ps_dis <- "TRUE"
  ite_method_inf <- "bcf"
  include_ps_inf <- "TRUE"
  ntrees_rf <- 100
  ntrees_gbm <- 50
  min_nodes <- 20
  max_nodes <- 5
  t <- 0.025
  q <- 0.8
  rules_method <- NA
  include_offset <- FALSE
  offset_name <- NA

  # Incorrect y, z, X input
  expect_error(cre(y = "test", z, X, ratio_dis, ite_method_dis, include_ps_dis,
                   ite_method_inf, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method,
                   include_offset, offset_name))
  expect_error(cre(y, z = "test", X, ratio_dis, ite_method_dis, include_ps_dis,
                   ite_method_inf, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method,
                   include_offset, offset_name))
  expect_error(cre(y, z, X = "test", ratio_dis, ite_method_dis, include_ps_dis,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method,
                   include_offset, offset_name))

  # Incorrect ratio_dis input
  expect_error(cre(y, z, X, ratio_dis = NA, ite_method_dis, include_ps_dis,
                   ite_method_inf, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method,
                   include_offset, offset_name))
  expect_error(cre(y, z, X, ratio_dis = 2, ite_method_dis, include_ps_dis,
                   ite_method_inf, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method,
                   include_offset, offset_name))

  # Incorrect ite_method input
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis = 0, include_ps_dis,
                   ite_method_inf, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method,
                   include_offset, offset_name))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis = "test", include_ps_dis,
                   ite_method_inf, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method,
                   include_offset, offset_name))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, include_ps_dis,
                   ite_method_inf = 0, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method,
                   include_offset, offset_name))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, include_ps_dis,
                   ite_method_inf = "test", include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method,
                   include_offset, offset_name))

  # Incorrect ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q inputs
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ite_method_inf, include_ps_inf,
                   ntrees_rf = "test", ntrees_gbm,
                   min_nodes, max_nodes, t, q, rules_method,
                   include_offset, offset_name))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ite_method_inf, include_ps_inf,
                   ntrees_rf, ntrees_gbm = "test",
                   min_nodes, max_nodes, t, q, rules_method,
                   include_offset, offset_name))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ite_method_inf, include_ps_inf,
                   ntrees_rf, ntrees_gbm,
                   min_nodes = "test", max_nodes, t, q, rules_method,
                   include_offset, offset_name))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ite_method_inf, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes = "test", t, q, rules_method,
                   include_offset, offset_name))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ite_method_inf, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t = "test", q, rules_method,
                   include_offset, offset_name))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ite_method_inf, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t, q = "test", rules_method,
                   include_offset, offset_name))

  # Correct outputs
  cre_results <- cre(y, z, X, ratio_dis, ite_method_dis, include_ps_dis, ite_method_inf, include_ps_inf,
                     ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method,
                     include_offset, offset_name)
  expect_true(class(cre_results) == "data.frame")
})
