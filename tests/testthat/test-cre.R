test_that("CRE Runs Correctly", {
  # Generate sample data
  dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, effect_size = 0.5, discrete = FALSE, seed = 2021)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  ratio_dis <- 0.25
  ite_method_dis <- "xbart"
  ite_method_inf <- "xbcf"
  include_ps_dis <- "TRUE"
  include_ps_inf <- NA
  ntrees <- 100
  min_nodes <- 20
  max_nodes <- 5
  t <- 0.025

  # Incorrect y, z, X input
  expect_error(cre(y = "test", z, X, ratio_dis, ite_method_dis, ite_method_inf, include_ps_dis, include_ps_inf, ntrees, min_nodes, max_nodes, t))
  expect_error(cre(y, z = "test", X, ratio_dis, ite_method_dis, ite_method_inf, include_ps_dis, include_ps_inf, ntrees, min_nodes, max_nodes, t))
  expect_error(cre(y, z, X = "test", ratio_dis, ite_method_dis, ite_method_inf, include_ps_dis, include_ps_inf, ntrees, min_nodes, max_nodes, t))

  # Incorrect ratio_dis input
  expect_error(cre(y, z, X, ratio_dis = "test", ite_method_dis, ite_method_inf, include_ps_dis, include_ps_inf, ntrees, min_nodes, max_nodes, t))
  expect_error(cre(y, z, X, ratio_dis = 2, ite_method_dis, ite_method_inf, include_ps_dis, include_ps_inf, ntrees, min_nodes, max_nodes, t))

  # Incorrect ite_method input
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis = 0, ite_method_inf, include_ps_dis, include_ps_inf, ntrees, min_nodes, max_nodes, t))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis = "test", ite_method_inf, include_ps_dis, include_ps_inf, ntrees, min_nodes, max_nodes, t))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf = 0, include_ps_dis, include_ps_inf, ntrees, min_nodes, max_nodes, t))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf = "test", include_ps_dis, include_ps_inf, ntrees, min_nodes, max_nodes, t))

  # Incorrect include_ps input
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf, include_ps_dis = "test", include_ps_inf, ntrees, min_nodes, max_nodes, t))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf, include_ps_dis, include_ps_inf = "test", ntrees, min_nodes, max_nodes, t))

  # Incorrect ntrees, min_nodes, max_nodes, t inputs
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf, include_ps_dis, include_ps_inf, ntrees = "test", min_nodes, max_nodes, t))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf, include_ps_dis, include_ps_inf, ntrees, min_nodes = "test", max_nodes, t))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf, include_ps_dis, include_ps_inf, ntrees, min_nodes, max_nodes = "test", t))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf, include_ps_dis, include_ps_inf, ntrees, min_nodes, max_nodes, t = "test"))

  # Correct outputs
  cre_results <- cre(y, z, X, ratio_dis, ite_method_dis, ite_method_inf, include_ps_dis, include_ps_inf, ntrees, min_nodes, max_nodes, t)
  expect_true(class(cre_results) == "list")
  expect_true(length(cre_results) == 3)
  expect_true(class(cre_results[[1]]) == "character")
  expect_identical(class(cre_results[[2]]), c("data.frame"))
  expect_identical(class(cre_results[[3]]), c("data.frame"))
  expect_true(length(cre_results[[1]]) == nrow(cre_results[[2]]) - 1)
  expect_true(length(cre_results[[1]]) == nrow(cre_results[[3]]) - 1)
  expect_identical(names(cre_results[[2]]), c("Rule", "CATE", "CI_lower", "CI_upper"))
  expect_identical(names(cre_results[[3]]), c("Rule", "CATE", "PVal", "CI_lower", "CI_upper"))
  expect_true(cre_results[[2]][1,1] == "Average Treatment Effect")
  expect_true(cre_results[[3]][1,1] == "(Intercept)")
})
