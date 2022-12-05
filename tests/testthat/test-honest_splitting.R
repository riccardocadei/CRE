test_that("split_data works as expected.", {
  # Generate sample data
  set.seed(1321)
  dataset <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5, binary_outcome = FALSE)
  y <- dataset[["y"]]
  z <- dataset[["z"]]
  X <- dataset[["X"]]
  ratio_dis <- 0.25

  # Step 1: Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)

  # Incorrect data inputs
  expect_error(honest_splitting(y, z, X, ratio_dis = NA))
  expect_error(honest_splitting(y, z, X, ratio_dis = 2))

  # Correct outputs
  subgroups <- honest_splitting(y, z, X, ratio_dis)
  expect_true(length(subgroups) == 2)
  expect_identical(class(subgroups[[1]]), 'list')
  expect_identical(class(subgroups[[2]]), 'list')
  expect_true(nrow(subgroups[[1]]$X) == ratio_dis * nrow(X))
  expect_true(nrow(subgroups[[2]]$X) == (1 - ratio_dis) * nrow(X))

  # Values
  discovery <- subgroups[["discovery"]]
  inference <- subgroups[["inference"]]
  y_dis <- discovery$y
  z_dis <- discovery$z
  X_dis <- discovery$X

  y_inf <- inference$y
  z_inf <- inference$z
  X_inf <- inference$X

  expect_equal(y_dis[2], -0.4007489, tolerance = 0.00001)
  expect_equal(z_inf[10], 0, tolerance = 0.00001)
  expect_equal(X_dis[3,9][[1]], 1, tolerance = 0.00001)

})
