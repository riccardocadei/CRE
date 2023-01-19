test_that("CF ITE Estimated Correctly", {

  skip_if_not_installed("grf")

  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5,
                                       binary_outcome = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  include_ps <- TRUE
  ps_method <- "SL.xgboost"

  # Incorrect data inputs
  expect_error(estimate_ite_cf(y = "test", z, X, ps_method))
  expect_error(estimate_ite_cf(y, z = "test", X, ps_method))
  expect_error(estimate_ite_cf(y, z, X = NA, ps_method))
  expect_error(estimate_ite_cf(y, z, X, ps_method = NA))

  # Correct outputs
  ite <- estimate_ite_cf(y, z, X, ps_method)
  expect_true(length(ite) == length(y))
  expect_true(class(ite) == "numeric")
})
