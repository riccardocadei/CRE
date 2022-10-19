test_that("BCF ITE Estimated Correctly", {
  # Generate sample data
  set.seed(697)
  dataset_cont <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- as.matrix(dataset_cont[["X"]])
  include_ps <- TRUE
  ps_method <- "SL.xgboost"

  # Incorrect data inputs
  expect_error(estimate_ite_bcf(y = "test", z, X, ps_method))
  expect_error(estimate_ite_bcf(y, z = "test", X, ps_method))
  expect_error(estimate_ite_bcf(y, z, X = NA, ps_method))

  # Correct outputs
  ite_result <- estimate_ite_bcf(y, z, X, ps_method)
  expect_true(length(ite_result) == 2)
  expect_true(length(ite_result[[1]]) == length(y))
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(length(ite_result[[2]]) == length(y))
  expect_true(class(ite_result[[2]]) == "numeric")
})
