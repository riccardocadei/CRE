test_that("ITE Estimated Correctly", {

  #Generate sample data
  set.seed(181)
  dts_1 <- generate_cre_dataset(n = 300, rho = 0, n_rules = 2, p = 10,
                                effect_size = 2, binary_covariates = FALSE,
                                binary_outcome = FALSE)
  dts_2 <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                effect_size = 2, binary_outcome = FALSE)

  ite_method <- "bart"
  include_ps <- TRUE
  ps_method <- "SL.xgboost"
  oreg_method <- NA
  ntrees <- 100
  node_size <- 20
  max_nodes <- 5

  # Wrong ite estimator
  expect_error(estimate_ite(y = dts_2$y, z = dts_1$z, dts_1$X,
                            ite_method = "test",
                            include_ps = include_ps,
                            ps_method = ps_method,
                            oreg_method = oreg_method))

  # Missing arguments
  expect_error(estimate_ite(y = dts_2$y, z = dts_1$z, dts_1$X,
                            ite_method = "poisson",
                            include_ps = include_ps,
                            ps_method = ps_method,
                            oreg_method = oreg_method))

  # Wrong input size
  expect_error(estimate_ite(y = dts_2$y, z = dts_1$z, dts_1$X,
                              ite_method,
                              include_ps = include_ps,
                              ps_method = ps_method,
                              oreg_method = oreg_method))

  expect_warning(expect_error(estimate_ite(y = dts_1$y, z = dts_2$z, dts_1$X,
                              ite_method,
                              include_ps = include_ps,
                              ps_method = ps_method,
                              oreg_method = oreg_method)))

  expect_warning(expect_error(estimate_ite(y = dts_1$y, z = dts_1$z, dts_2$X,
                              ite_method,
                              include_ps = include_ps,
                              ps_method = ps_method,
                              oreg_method = oreg_method)))

  # Correct outputs
  ite_result <- estimate_ite(y = dts_1$y, z = dts_1$z, dts_1$X,
                             ite_method,
                             include_ps = include_ps,
                             ps_method = ps_method,
                             oreg_method = oreg_method)

  expect_true(length(ite_result) == 2)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(dts_1$y))


  ite_method <- "sipw"
  ite_result <- estimate_ite(y = dts_1$y, z = dts_1$z, dts_1$X,
                             ite_method,
                             include_ps = include_ps,
                             ps_method = ps_method,
                             oreg_method = oreg_method)

  expect_true(length(ite_result) == 2)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(dts_1$y))


  ite_method <- "bcf"
  ite_result <- estimate_ite(y = dts_1$y, z = dts_1$z, as.matrix(dts_1$X),
                             ite_method,
                             include_ps = include_ps,
                             ps_method = ps_method,
                             oreg_method = oreg_method)

  expect_true(length(ite_result) == 2)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(dts_1$y))

  ite_method <- "poisson"
  ite_result <- estimate_ite(y = round(abs(dts_1$y) + 1), z = dts_1$z, dts_1$X,
                             ite_method,
                             include_ps = include_ps,
                             ps_method = ps_method,
                             oreg_method = oreg_method,
                             random_state = random_state,
                             offset = NULL,
                             X_names = names(dts_1$X))

  expect_true(length(ite_result) == 2)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(dts_1$y))
})


test_that("ITE (oreg) Estimated Correctly", {

  skip_if_not_installed("BART")
  #Generate sample data
  set.seed(233)
  dts_1 <- generate_cre_dataset(n = 300, rho = 0, n_rules = 2, p = 10,
                                effect_size = 2, binary_outcome = FALSE)

  ite_method <- "bart"
  include_ps <- TRUE
  ps_method <- "SL.xgboost"
  oreg_method <- NA
  ntrees <- 100
  node_size <- 20
  max_nodes <- 5

  ite_method <- "oreg"
  ite_result <- estimate_ite(y = dts_1$y, z = dts_1$z, dts_1$X,
                             ite_method,
                             include_ps = include_ps,
                             ps_method = ps_method,
                             oreg_method = oreg_method,
                             random_state = random_state)

  expect_true(length(ite_result) == 2)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(dts_1$y))
})


test_that("ITE (cf) Estimated Correctly", {

  skip_if_not_installed("grf")

  ite_method <- "cf"
  include_ps <- TRUE
  ps_method <- "SL.xgboost"
  oreg_method <- NA
  ntrees <- 100
  node_size <- 20
  max_nodes <- 5
  random_state <- 121

  dts_1 <- generate_cre_dataset(n = 300, rho = 0, n_rules = 2, p = 10,
                                effect_size = 2, binary_outcome = TRUE)

  ite_result <- estimate_ite(y = abs(dts_1$y), z = dts_1$z, dts_1$X,
                             ite_method,
                             include_ps = include_ps,
                             ps_method = ps_method,
                             oreg_method = oreg_method,
                             random_state = random_state)

  expect_true(length(ite_result) == 2)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(dts_1$y))

  # TODO: add tests on standard deviation
})
