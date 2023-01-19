test_that("generate_cre_dataset works as expected.", {

  # Incorrect n input
  expect_error(generate_cre_dataset(n = -100, rho = 0,
                                    n_rules = 2, p = 10, effect_size = 0.5,
                                    binary_outcome = FALSE))

  # Incorrect rho input (negative value)
  expect_error(generate_cre_dataset(n = 100, rho = -0.5,
                                    n_rules = 2, p = 10, effect_size = 0.5,
                                    binary_outcome = FALSE))

  # Incorrect rho input (more than 1)
  expect_error(generate_cre_dataset(n = 100, rho = 1.2,
                                    n_rules = 2, p = 10, effect_size = 0.5,
                                    binary_outcome = FALSE))

  # Incorrect number of rules
  expect_error(generate_cre_dataset(n = 100, rho = 0,
                                    n_rules = 5, p = 10, effect_size = 0.5,
                                    binary_outcome = FALSE))

  expect_error(generate_cre_dataset(n = 100, rho = 0,
                                    n_rules = "test", p = 10, effect_size = 0.5,
                                    binary_outcome = FALSE))

  expect_error(generate_cre_dataset(n = 100, rho = 0,
                                    n_rules = 3, p = 10, effect_size = 0.5,
                                    binary_outcome = TRUE))

  # Incorrect effect_size input
  expect_error(generate_cre_dataset(n = 100, rho = 0,
                                    n_rules = 2, p = 10, effect_size = "test",
                                    binary_outcome = FALSE))

  # Incorrect binary input
  expect_error(generate_cre_dataset(n = 100, rho = 0,
                                    n_rules = 2, p = 10, effect_size = 0.5,
                                    binary_outcome = "test"))

  # Correct outputs
  set.seed(2021)
  test_data_1 <- generate_cre_dataset(n = 100, rho = 0, n_rules = 1, p = 10,
                                      effect_size = 0.5, binary_outcome = TRUE)
  expect_true(class(test_data_1) == "list")
  expect_true(length(test_data_1) == 4)
  expect_true(class(test_data_1[[1]]) == "numeric")
  expect_true(class(test_data_1[[2]]) == "integer")
  expect_identical(class(test_data_1[[3]]), "data.frame")
  expect_true(length(test_data_1[[1]]) == 100)
  expect_true(length(test_data_1[[2]]) == 100)
  expect_true(nrow(test_data_1[[3]]) == 100)
  expect_true(ncol(test_data_1[[3]]) == 10)
  expect_equal(test_data_1$y[75], 0)
  expect_equal(test_data_1$X[4, 4], 1)

  test_data_1 <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                      effect_size = 0.5, binary_outcome = FALSE)
  expect_true(class(test_data_1) == "list")
  expect_true(length(test_data_1) == 4)
  expect_true(class(test_data_1[[1]]) == "numeric")
  expect_true(class(test_data_1[[2]]) == "integer")
  expect_identical(class(test_data_1[[3]]), "data.frame")
  expect_true(length(test_data_1[[1]]) == 100)
  expect_true(length(test_data_1[[2]]) == 100)
  expect_true(nrow(test_data_1[[3]]) == 100)
  expect_true(ncol(test_data_1[[3]]) == 10)
  expect_equal(test_data_1$y[75], -0.1272132, tolerance = 0.000001)
  expect_equal(test_data_1$X[4, 4], 0)

  test_data_1 <- generate_cre_dataset(n = 100, rho = 0, n_rules = 3, p = 10,
                                      effect_size = 0.5, binary_outcome = FALSE)
  expect_true(class(test_data_1) == "list")
  expect_true(length(test_data_1) == 4)
  expect_true(class(test_data_1[[1]]) == "numeric")
  expect_true(class(test_data_1[[2]]) == "integer")
  expect_identical(class(test_data_1[[3]]), "data.frame")
  expect_true(length(test_data_1[[1]]) == 100)
  expect_true(length(test_data_1[[2]]) == 100)
  expect_true(nrow(test_data_1[[3]]) == 100)
  expect_true(ncol(test_data_1[[3]]) == 10)
  expect_equal(test_data_1$y[75], 1.650115, tolerance = 0.000001)
  expect_equal(test_data_1$X[4, 4], 0)

  test_data_1 <- generate_cre_dataset(n = 100, rho = 0, n_rules = 4, p = 10,
                                      effect_size = 0.5, binary_outcome = FALSE)
  expect_true(class(test_data_1) == "list")
  expect_true(length(test_data_1) == 4)
  expect_true(class(test_data_1[[1]]) == "numeric")
  expect_true(class(test_data_1[[2]]) == "integer")
  expect_identical(class(test_data_1[[3]]), "data.frame")
  expect_true(length(test_data_1[[1]]) == 100)
  expect_true(length(test_data_1[[2]]) == 100)
  expect_true(nrow(test_data_1[[3]]) == 100)
  expect_true(ncol(test_data_1[[3]]) == 10)
  expect_equal(test_data_1$y[75], 0.3763141, tolerance = 0.000001)
  expect_equal(test_data_1$X[4, 4], 0)
})
