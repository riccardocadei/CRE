test_that("split_data works as expected.", {
  # Generate sample data
  set.seed(1321)
  dataset <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5, binary = FALSE)
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
  expect_identical(class(subgroups[[1]]), c("matrix", "array"))
  expect_identical(class(subgroups[[2]]), c("matrix", "array"))
  expect_true(nrow(subgroups[[1]]) == ratio_dis * nrow(X))
  expect_true(nrow(subgroups[[2]]) == (1 - ratio_dis) * nrow(X))

  # Values
  discovery <- subgroups[["discovery"]]
  inference <- subgroups[["inference"]]
  y_dis <- discovery[,1]
  z_dis <- discovery[,2]
  X_dis <- discovery[,3:ncol(discovery)]

  y_inf <- inference[,1]
  z_inf <- inference[,2]
  X_inf <- inference[,3:ncol(inference)]

  expect_equal(y_dis[2], 1.842319, tolerance = 0.00001)
  expect_equal(z_inf[10], 1, tolerance = 0.00001)
  expect_equal(X_dis[3,9][[1]], 0.9244026, tolerance = 0.00001)

})
