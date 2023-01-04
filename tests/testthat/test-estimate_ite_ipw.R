test_that("IPW ITE Estimated Correctly", {
  # Generate sample data
  set.seed(1769)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5,
                                       binary_outcome = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  ps_method <- "SL.xgboost"

  # Incorrect data inputs
  expect_error(estimate_ite_ipw(y = "test", z, X, ps_method))
  expect_error(estimate_ite_ipw(y, z = "test", X, ps_method))
  expect_error(estimate_ite_ipw(y, z, X = NA, ps_method))

  # Correct outputs
  ite_result <- estimate_ite_ipw(y, z, X, ps_method)
  expect_true(length(ite_result) == length(y))
  expect_true(class(ite_result) == "numeric")

  # Reproducible results
  expect_equal(ite_result[3], -2.44723, tolerance = 0.000001)
  expect_equal(ite_result[69], 2.512476, tolerance = 0.000001)
})
