test_that("AIPW ITE Estimated Correctly", {
  # Generate sample data
  set.seed(8697)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  ps_method <- "SL.xgboost"
  or_method <- "SL.xgboost"

  # Incorrect data inputs
  expect_error(estimate_ite_aipw(y = "test", z, X, ps_method, or_method))
  expect_error(estimate_ite_aipw(y, z = "test", X, ps_method, or_method))
  expect_error(estimate_ite_aipw(y, z, X = NA, ps_method, or_method))

  # Correct outputs
  ite_result <- estimate_ite_aipw(y, z, X, ps_method, or_method)
  expect_true(length(ite_result) == length(y))
  expect_true(class(ite_result) == "numeric")

  # Reproducible results
  expect_equal(ite_result[1], 0.003285416, tolerance = 0.00001)
  expect_equal(ite_result[11], -0.01266725, tolerance = 0.00001)
  expect_equal(ite_result[91], 0.03005526, tolerance = 0.00001)
})

