test_that("CRE Runs Correctly", {
  # Generate sample data
  dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2,
                                       effect_size = 0.5, binary = TRUE, seed = 2021)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  ratio_dis <- 0.25
  ite_method_dis <- "xbart"
  ite_method_inf <- "xbart"
  include_ps_dis <- TRUE
  include_ps_inf <- TRUE
  ntrees_rf <- 100
  ntrees_gbm <- 50
  min_nodes <- 20
  max_nodes <- 5
  t <- 0.025
  q <- 0.8
  rules_method <- "conservative"

  # Incorrect y, z, X input
  expect_error(cre(y = "test", z, X, ratio_dis, ite_method_dis,
                   ite_method_inf, include_ps_dis, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method))
  expect_error(cre(y, z = "test", X, ratio_dis, ite_method_dis,
                   ite_method_inf, include_ps_dis, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method))
  expect_error(cre(y, z, X = "test", ratio_dis, ite_method_dis,
                   ite_method_inf, include_ps_dis, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method))

  # Incorrect ratio_dis input
  expect_error(cre(y, z, X, ratio_dis = NA, ite_method_dis,
                   ite_method_inf, include_ps_dis, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method))
  expect_error(cre(y, z, X, ratio_dis = 2, ite_method_dis,
                   ite_method_inf, include_ps_dis, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method))

  # Incorrect ite_method input
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis = 0,
                   ite_method_inf, include_ps_dis, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis = "test",
                   ite_method_inf, include_ps_dis, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   ite_method_inf = 0, include_ps_dis, include_ps_inf,
                   ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   ite_method_inf = "test", include_ps_dis,
                   include_ps_inf, ntrees_rf, ntrees_gbm, min_nodes, max_nodes,
                   t, q, rules_method))

  # Incorrect include_ps input
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf,
                   include_ps_dis = "test", include_ps_inf, ntrees_rf, ntrees_gbm,
                   min_nodes, max_nodes, t, q, rules_method))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf,
                   include_ps_dis, include_ps_inf = "test", ntrees_rf, ntrees_gbm,
                   min_nodes, max_nodes, t, q, rules_method))

  # Incorrect ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q inputs
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf,
                   include_ps_dis, include_ps_inf, ntrees_rf = "test", ntrees_gbm,
                   min_nodes, max_nodes, t, q, rules_method))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf,
                   include_ps_dis, include_ps_inf, ntrees_rf, ntrees_gbm = "test",
                   min_nodes, max_nodes, t, q, rules_method))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf,
                   include_ps_dis, include_ps_inf, ntrees_rf, ntrees_gbm,
                   min_nodes = "test", max_nodes, t, q, rules_method))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf,
                   include_ps_dis, include_ps_inf, ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes = "test", t, q, rules_method))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf,
                   include_ps_dis, include_ps_inf, ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t = "test", q, rules_method))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf,
                   include_ps_dis, include_ps_inf, ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t, q = "test", rules_method))

  # Incorrect rules_method input
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf,
                   include_ps_dis, include_ps_inf, ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t, q, rules_method = "test"))

  # Correct outputs
  cre_results <- cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf, include_ps_dis,
                     include_ps_inf, ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q, rules_method)
  expect_true(class(cre_results) == "list")
  expect_true(length(cre_results) == 2)
  expect_true(class(cre_results[[1]]) == "character")
  expect_identical(class(cre_results[[2]]), c("data.frame"))
  expect_true(length(cre_results[[1]]) == nrow(cre_results[[2]]) - 1)
  expect_identical(names(cre_results[[2]]), c("Rule", "Model_Coef", "CATE", "PVal", "CI_lower", "CI_upper"))
  expect_true(cre_results[[2]][1,1] == "(Intercept)")
})
