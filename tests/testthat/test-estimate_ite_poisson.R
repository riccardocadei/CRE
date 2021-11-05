test_that("Poisson ITE Estimated Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2,
                                       effect_size = 0.5, binary = FALSE)
  y <- abs(dataset_cont[["y"]])
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  X_names <- names(X)
  include_offset <- FALSE
  offset_name <- NA

  # Incorrect data inputs
  expect_error(estimate_ite_poisson(y = "test", z, X, X_names, include_offset, offset_name))
  expect_error(estimate_ite_poisson(y, z = "test", X, X_names, include_offset, offset_name))
  expect_error(estimate_ite_poisson(y, z, X = NA, X_names, include_offset, offset_name))

  # Correct outputs
  ite_result <- estimate_ite_poisson(y, z, X, X_names, include_offset, offset_name)
  expect_true(class(ite_result) == "numeric")
  expect_true(length(ite_result) == length(y))
})
