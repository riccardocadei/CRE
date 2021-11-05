test_that("SIPW ITE Estimated Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, effect_size = 0.5, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]

  # Incorrect data inputs
  expect_error(estimate_ite_or(y = "test", z, X))
  expect_error(estimate_ite_or(y, z = NA, X))
  expect_error(estimate_ite_or(y, z, X = NA))

  # Correct outputs
  ite_result <- estimate_ite_or(y, z, X)
  expect_true(length(ite_result) == length(y))
  expect_true(class(ite_result) == "numeric")
})
