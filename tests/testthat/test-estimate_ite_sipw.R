test_that("SIPW ITE Estimated Correctly", {

  # Generate sample data
  set.seed(1789697)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  ps_method <- "SL.xgboost"

  # Incorrect data inputs
  expect_error(estimate_ite_sipw(y = "test", z, X, ps_method))
  expect_error(estimate_ite_sipw(y, z = NA, X, ps_method))
  expect_error(estimate_ite_sipw(y, z, X = NA, ps_method))

  # Correct outputs
  ite_result <- estimate_ite_sipw(y, z, X, ps_method)
  expect_true(length(ite_result) == length(y))
  expect_true(class(ite_result) == "numeric")

  # Reproducible results
  expect_equal(ite_result[1], 0.7699422, tolerance = 0.000001)
  expect_equal(ite_result[19], -2.015022, tolerance = 0.000001)
  expect_equal(ite_result[72], -2.026895, tolerance = 0.000001)
})
