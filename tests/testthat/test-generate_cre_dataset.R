test_that("generate_cre_dataset works as expected.", {

  # Incorrect n input
  expect_error(generate_cre_dataset(n = -100, rho = 0,
                                    n_rules = 2, p = 10, effect_size = 0.5,
                                    binary = FALSE))

  # Incorrect rho input (negative value)
  expect_error(generate_cre_dataset(n = 100, rho = -0.5,
                                    n_rules = 2, p = 10, effect_size = 0.5,
                                    binary = FALSE))

  # Incorrect rho input (more than 1)
  expect_error(generate_cre_dataset(n = 100, rho = 1.2,
                                    n_rules = 2, p = 10, effect_size = 0.5,
                                    binary = FALSE))

  # Incorrect number of rules
  expect_error(generate_cre_dataset(n = 100, rho = 0,
                                    n_rules = 3, p = 10, effect_size = 0.5,
                                    binary = FALSE))

  expect_error(generate_cre_dataset(n = 100, rho = 0,
                                    n_rules = "test", p = 10, effect_size = 0.5,
                                    binary = FALSE))

  # Incorrect effect_size input
  expect_error(generate_cre_dataset(n = 100, rho = 0,
                                    n_rules = 2, p = 10, effect_size = "test",
                                    binary = FALSE))

  # Incorrect binary input
  expect_error(generate_cre_dataset(n = 100, rho = 0,
                                    n_rules = 2, p = 10, effect_size = 0.5,
                                    binary = "test"))

  # Correct outputs
  set.seed(2021)
  test_data_1 <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                         effect_size = 0.5, binary = TRUE)
  expect_true(class(test_data_1) == "list")
  expect_true(length(test_data_1) == 3)
  expect_true(class(test_data_1[[1]]) == "numeric")
  expect_true(class(test_data_1[[2]]) == "integer")
  expect_identical(class(test_data_1[[3]]), "data.frame")
  expect_true(length(test_data_1[[1]]) == 100)
  expect_true(length(test_data_1[[2]]) == 100)
  expect_true(nrow(test_data_1[[3]]) == 100)
  expect_true(ncol(test_data_1[[3]]) == 10)
  expect_equal(test_data_1$y[75], 1)
  expect_equal(test_data_1$X[4,4], 1)

  # Correct outputs
  set.seed(2021)
  test_data_1 <- generate_cre_dataset(n = 100, rho = 0, n_rules = 4, p = 10,
                                      effect_size = 0.5, binary = FALSE)
  expect_true(class(test_data_1) == "list")
  expect_true(length(test_data_1) == 3)
  expect_true(class(test_data_1[[1]]) == "numeric")
  expect_true(class(test_data_1[[2]]) == "integer")
  expect_identical(class(test_data_1[[3]]), "data.frame")
  expect_true(length(test_data_1[[1]]) == 100)
  expect_true(length(test_data_1[[2]]) == 100)
  expect_true(nrow(test_data_1[[3]]) == 100)
  expect_true(ncol(test_data_1[[3]]) == 10)
  expect_equal(test_data_1$y[75], 0.5411234, tolerance = 0.000001)
  expect_equal(test_data_1$X[4,4], 1)
})
