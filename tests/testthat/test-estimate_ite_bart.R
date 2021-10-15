test_that("BART ITE Estimated Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2,
                                       effect_size = 0.5, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  include_ps <- TRUE

  # Incorrect data inputs
  expect_error(estimate_ite_bart(y = "test", z, X, include_ps))
  expect_error(estimate_ite_bart(y, z = "test", X, include_ps))
  expect_error(estimate_ite_bart(y, z, X = "test", include_ps))
  expect_error(estimate_ite_bart(y, z, X = "test", include_ps = "test"))
  expect_error(estimate_ite_bart(y, z, X = "test", include_ps = NA))

  # Correct outputs
  ite_result <- estimate_ite_bart(y, z, X, include_ps)
  expect_true(length(ite_result) == 2)
  expect_true(length(ite_result[[1]]) == length(y))
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(length(ite_result[[2]]) == length(y))
  expect_true(class(ite_result[[2]]) == "numeric")
})
