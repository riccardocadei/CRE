test_that("Poisson ITE Estimated Correctly", {
  # Generate sample data
  set.seed(8967)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5, binary_covariates = FALSE,
                                       binary_outcome = FALSE)
  y <- trunc(abs(dataset_cont[["y"]]) * 10)
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  X_names <- names(X)
  offset <- NULL

  # Incorrect data inputs
  expect_error(estimate_ite_poisson(y = "test", z, X, X_names, offset))
  expect_warning(expect_error(estimate_ite_poisson(y, z = "test", X, X_names,
                                                   offset)))
  expect_error(estimate_ite_poisson(y, z, X = NA, X_names, offset))
  expect_error(estimate_ite_poisson(y, z, X, X_names, offset="x_1"))

  # Correct outputs
  # Poisson (vanila)
  ite_result <- estimate_ite_poisson(y, z, X, X_names, offset)
  expect_true(class(ite_result) == "numeric")
  expect_true(length(ite_result) == length(y))

  # Poisson + Offset
  offset <- "x9"
  ite_result <- estimate_ite_poisson(y, z, X, X_names, offset)
  expect_true(class(ite_result) == "numeric")
  expect_true(length(ite_result) == length(y))

  # Reproducible results
  expect_equal(ite_result[[1]], 0.5626742, tolerance = 0.000001)
  expect_equal(ite_result[[19]], -0.3934483, tolerance = 0.000001)
  expect_equal(ite_result[[72]], 0.8419285, tolerance = 0.000001)
})
