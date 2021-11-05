test_that("BCF ITE Estimated Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2,
                                       effect_size = 0.5, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  include_ps <- TRUE

  # Incorrect data inputs
  expect_error(estimate_ite_bcf(y = "test", z, X))
  expect_error(estimate_ite_bcf(y, z = "test", X))
  expect_error(estimate_ite_bcf(y, z, X = NA))

  # Correct outputs
  ite_result <- estimate_ite_bcf(y, z, X)
  expect_true(length(ite_result) == 2)
  expect_true(length(ite_result[[1]]) == length(y))
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(length(ite_result[[2]]) == length(y))
  expect_true(class(ite_result[[2]]) == "numeric")
})
