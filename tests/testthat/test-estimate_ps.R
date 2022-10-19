test_that("Propensity Score Estimated Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5, binary = FALSE)
  z <- dataset[["z"]]
  X <- dataset[["X"]]
  ps_method <- "SL.xgboost"

  # Incorrect data inputs
  expect_error(estimate_ps(z = "test", X, ps_method))
  expect_error(estimate_ps(z, X = NA, ps_method))

  # Correct outputs
  est_ps <- estimate_ps(z, X, ps_method)
  expect_true(length(est_ps) == length(z))
  expect_true(class(est_ps) == "numeric")
  expect_true(is.vector(est_ps))

  #values
  expect_equal(est_ps[2], 0.4805386, tolerance = 0.00001)
  expect_equal(est_ps[37], 0.149296, tolerance = 0.00001)
})
