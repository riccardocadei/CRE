test_that("Propensity Score Estimated Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5, binary = FALSE)
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  ps_method <- "SL.xgboost"

  # Incorrect data inputs
  expect_error(estimate_ps(z = "test", X, ps_method))
  expect_error(estimate_ps(z, X = NA, ps_method))

  # Correct outputs
  est_ps <- estimate_ps(z, X, ps_method)
  expect_true(length(est_ps) == length(z))
  expect_true(class(est_ps) == "numeric")
})
