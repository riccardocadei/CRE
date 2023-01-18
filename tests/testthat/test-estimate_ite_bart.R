test_that("BART ITE Estimated Correctly", {

  # Generate sample data
  set.seed(39678)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5,
                                       binary_outcome = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  include_ps <- TRUE
  ps_method <- "SL.xgboost"

  # Incorrect data inputs
  expect_error(estimate_ite_bart(y = "test", z, X, ps_method))
  expect_error(estimate_ite_bart(y, z = "test", X, ps_method))
  expect_error(estimate_ite_bart(y, z, X = NA, ps_method))
  expect_error(estimate_ite_bart(y, z, X, ps_method = NA))

  # Correct outputs
  ite <- estimate_ite_bart(y, z, X, ps_method)
  expect_true(length(ite) == length(y))
  expect_true(class(ite) == "numeric")
})
