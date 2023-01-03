test_that("BART ITE Estimated Correctly", {

  # Generate sample data
  set.seed(39678)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5, binary_outcome = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  include_ps <- TRUE
  ps_method <- "SL.xgboost"
  random_state <- 761

  # Incorrect data inputs
  expect_error(estimate_ite_bart(y = "test", z, X, include_ps, ps_method))
  expect_error(estimate_ite_bart(y, z = "test", X, include_ps, ps_method))
  expect_error(estimate_ite_bart(y, z, X = NA, include_ps, ps_method))
  expect_error(estimate_ite_bart(y, z, X = NA, include_ps = "test", ps_method))
  expect_error(estimate_ite_bart(y, z, X = NA, include_ps = NA, ps_method))

  # Correct outputs
  set.seed(9976)
  ite_result <- estimate_ite_bart(y, z, X, include_ps, ps_method)
  expect_true(length(ite_result) == 2)
  expect_true(length(ite_result[[1]]) == length(y))
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(length(ite_result[[2]]) == length(y))
  expect_true(class(ite_result[[2]]) == "numeric")

  # Reproducible results
  # TODO: bart results are not reproducible.
  # expect_equal(ite_result[[1]][10], -0.3719706, tolerance = 0.00001)
  # expect_equal(ite_result[[1]][92], -1.458027, tolerance = 0.00001)
  # expect_equal(ite_result[[2]][23], 0.9523162, tolerance = 0.00001)
  # expect_equal(ite_result[[2]][74], 0.9747365, tolerance = 0.00001)
})
