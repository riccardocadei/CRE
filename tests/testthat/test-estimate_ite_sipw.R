test_that("SIPW ITE Estimated Correctly", {
  # Generate sample data
  set.seed(2021)
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
})
