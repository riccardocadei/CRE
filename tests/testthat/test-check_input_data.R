test_that("check_input_data works as expected.", {

  ds_1 <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                  effect_size = 2, binary = FALSE)

  ds_2 <- generate_cre_dataset(n = 200, rho = 0, n_rules = 2, p = 10,
                                  effect_size = 2, binary = FALSE)

  # invalid size
  expect_error(check_input_data(ds_1$y, ds_2$z, ds_2$X))
  expect_error(check_input_data(ds_1$y, ds_1$z, ds_2$X))

  # invalid data type
  l_y <- sample(letters, 100, replace = TRUE)
  expect_error(check_input_data(l_y, ds_1$z, ds_1$X))

  # non-binary treatment
  z_3 <- sample(c(1,2,3), 100, replace = TRUE)
  expect_error(check_input_data(ds_1$y, z_3, ds_1$X))

})
