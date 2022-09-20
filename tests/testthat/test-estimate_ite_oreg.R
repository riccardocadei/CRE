test_that("OREG ITE Estimated Correctly", {
  # Generate sample data
  set.seed(17894)
  dataset_cont <- generate_cre_dataset(n = 50, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]

  # Incorrect data inputs
  expect_error(estimate_ite_oreg(y = NA, z, X))
  expect_error(estimate_ite_oreg(y, z = NA, X))
  expect_error(estimate_ite_oreg(y, z, X = NA))

  # Correct outputs
  ite_result <- estimate_ite_oreg(y, z, X)
  expect_true(length(ite_result) == length(y))
  expect_true(class(ite_result) == "numeric")

  # Values
  expect_equal(ite_result[12], -0.4560013691, tolerance = 0.00001)

})
