test_that("Poisson ITE Estimated Correctly", {
  # Generate sample data
  set.seed(8967)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5,
                                       binary_covariates = FALSE,
                                       binary_outcome = FALSE)
  y <- trunc(abs(dataset_cont[["y"]]) * 10)
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  offset <- NULL

  # Incorrect data inputs
  expect_error(estimate_ite_poisson(y = "test", z, X, offset))
  expect_warning(expect_error(estimate_ite_poisson(y, z = "test", X, offset)))
  expect_error(estimate_ite_poisson(y, z, X = NA, offset))
  expect_error(estimate_ite_poisson(y, z, X, offset = "x_1"))

  # Correct outputs
  # Poisson (vanila)
  ite_result <- estimate_ite_poisson(y, z, X, offset)
  expect_true(class(ite_result) == "numeric")
  expect_true(length(ite_result) == length(y))

  # Poisson + Offset
  offset <- "x9"
  ite_result <- estimate_ite_poisson(y, z, X, offset)
  expect_true(class(ite_result) == "numeric")
  expect_true(length(ite_result) == length(y))

  # Reproducible results
  expect_equal(ite_result[[1]], 0.9090112, tolerance = 0.000001)
  expect_equal(ite_result[[19]], -2.840591, tolerance = 0.000001)
  expect_equal(ite_result[[72]], 0.9013247, tolerance = 0.000001)
})
