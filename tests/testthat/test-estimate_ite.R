test_that("ITE Estimated Correctly", {

  #Generate sample data
  set.seed(181)
  dts_1 <- generate_cre_dataset(n = 300, rho = 0, n_rules = 2, p = 10,
                                effect_size = 2, binary_covariates = FALSE,
                                binary_outcome = FALSE)
  dts_2 <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                effect_size = 2, binary_covariates = FALSE,
                                binary_outcome = FALSE)

  ite_method <- "bart"
  learner_ps <- "SL.xgboost"
  learner_y <- "SL.xgboost"
  ntrees <- 100
  node_size <- 20
  max_nodes <- 5

  # Wrong ite estimator
  expect_error(estimate_ite(y = dts_1$y, z = dts_1$z, X = dts_1$X,
                            ite_method = "test",
                            learner_ps = learner_ps,
                            learner_y = learner_y))

  # Wrong input size
  expect_error(estimate_ite(y = dts_2$y, z = dts_1$z, X = dts_1$X,
                            ite_method,
                            learner_ps = learner_ps,
                            learner_y = learner_y))

  expect_warning(expect_error(estimate_ite(y = dts_1$y,
                                           z = dts_2$z,
                                           X = dts_1$X,
                                           ite_method,
                                           learner_ps = learner_ps,
                                           learner_y = learner_y)))

  expect_warning(expect_error(estimate_ite(y = dts_1$y,
                                           z = dts_1$z,
                                           X = dts_2$X,
                                           ite_method,
                                           learner_ps = learner_ps,
                                           learner_y = learner_y)))

  # Correct outputs
  ite_method <- "slearner"
  ite <- estimate_ite(y = dts_1$y, z = dts_1$z, X = dts_1$X,
                      ite_method,
                      learner_y = learner_y)
  expect_true(class(ite) == "numeric")
  expect_true(length(ite) == length(dts_1$y))

  ite_method <- "tlearner"
  ite <- estimate_ite(y = dts_1$y, z = dts_1$z, X = dts_1$X,
                      ite_method,
                      learner_y = learner_y)
  expect_true(class(ite) == "numeric")
  expect_true(length(ite) == length(dts_1$y))

  ite_method <- "xlearner"
  ite <- estimate_ite(y = dts_1$y, z = dts_1$z, X = dts_1$X,
                      ite_method,
                      learner_y = learner_y)
  expect_true(class(ite) == "numeric")
  expect_true(length(ite) == length(dts_1$y))

  ite_method <- "bart"
  ite <- estimate_ite(y = dts_1$y, z = dts_1$z, X = dts_1$X,
                      ite_method,
                      learner_ps = learner_ps,
                      learner_y = learner_y)
  expect_true(class(ite) == "numeric")
  expect_true(length(ite) == length(dts_1$y))

  ite_method <- "tpoisson"
  ite <- estimate_ite(y = round(abs(dts_1$y) + 1), z = dts_1$z, X = dts_1$X,
                      ite_method,
                      learner_ps = learner_ps,
                      learner_y = learner_y,
                      offset = NULL)
  expect_true(class(ite) == "numeric")
  expect_true(length(ite) == length(dts_1$y))

  ite_method <- "cf"
  ite <- estimate_ite(y = dts_1$y, z = dts_1$z, X = dts_1$X,
                      ite_method,
                      learner_ps = learner_ps,
                      learner_y = learner_y)
  expect_true(class(ite) == "numeric")
  expect_true(length(ite) == length(dts_1$y))
})
