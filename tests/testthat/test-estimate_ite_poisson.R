test_that("Poisson ITE Estimated Correctly", {
  # Generate sample data
  set.seed(8967)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5, binary = FALSE)
  y <- trunc(abs(dataset_cont[["y"]]) * 10)
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  X_names <- names(X)
  include_offset <- FALSE
  offset_name <- NA

  # Incorrect data inputs
  expect_error(estimate_ite_poisson(y = "test", z, X, X_names, include_offset,
                                    offset_name))
  expect_warning(expect_error(estimate_ite_poisson(y, z = "test", X, X_names,
                                                   include_offset,
                                                   offset_name)))
  expect_error(estimate_ite_poisson(y, z, X = NA, X_names, include_offset,
                                    offset_name))

  # Correct outputs
  # Poisson (vanila)
  ite_result <- estimate_ite_poisson(y, z, X, X_names, include_offset,
                                     offset_name)
  expect_true(class(ite_result) == "numeric")
  expect_true(length(ite_result) == length(y))

  # Poisson + Offset
  include_offset <- TRUE
  offset_name <- "x1"
  ite_result <- estimate_ite_poisson(y, z, X, X_names, include_offset,
                                     offset_name)
  expect_true(class(ite_result) == "numeric")
  expect_true(length(ite_result) == length(y))

  # Reproducible results
  expect_equal(ite_result[[1]], -0.8168112, tolerance = 0.000001)
  expect_equal(ite_result[[20]], -0.2781798, tolerance = 0.000001)
  expect_equal(ite_result[[91]], -0.2147065, tolerance = 0.000001)

})
