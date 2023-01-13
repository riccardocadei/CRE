test_that("T-Learner ITE Estimated Correctly", {
  # Generate sample data
  set.seed(8697)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5,
                                       binary_outcome = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  or_method <- "SL.xgboost"

  # Incorrect data inputs
  expect_error(estimate_ite_tlearner(y = "test", z, X, or_method))
  expect_error(estimate_ite_tlearner(y, z = "test", X, or_method))
  expect_error(estimate_ite_tlearner(y, z, X = NA, or_method))

  # Correct outputs
  ite <- expect_warning(estimate_ite_tlearner(y, z, X, or_method))
  expect_true(length(ite) == length(y))
  expect_true(class(ite) == "numeric")

  # Reproducible results
  expect_equal(ite[1], 0.8611812, tolerance = 0.00001)
  expect_equal(ite[11], -1.713489, tolerance = 0.00001)
  expect_equal(ite[91], -0.4767654, tolerance = 0.00001)
})
