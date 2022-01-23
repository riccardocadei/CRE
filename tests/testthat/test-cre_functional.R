test_that("CRE is Accurate", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- as.data.frame(dataset_cont[["X"]])
  X_names <- names(as.data.frame(X))
  ratio_dis <- 0.25
  ntrees_rf <- 100
  ntrees_gbm <- 50
  min_nodes <- 20
  max_nodes <- 5
  t <- 0.025
  q <- 0.8
  rules_method <- NA
  include_offset <- FALSE
  offset_name <- NA
  filter_cate <- FALSE

  # Poisson CATE estimation with Poisson ITE estimation
  print("Testing Poisson CATE estimation with Poisson ITE estimation")
  cre_results_1 <- cre(y = abs(y), z, X, ratio_dis, ite_method_dis = "poisson",
                       include_ps_dis = NA, ps_method_dis = "SL.xgboost",
                       or_method_dis = NA, ite_method_inf = "poisson",
                       include_ps_inf = NA, ps_method_inf = "SL.xgboost",
                       or_method_inf = NA, ntrees_rf, ntrees_gbm, min_nodes,
                       max_nodes, t, q, rules_method, include_offset, offset_name,
                       cate_method = "poisson", cate_SL_library = NA)
  expect_true(any(grepl("x1<=0.5 & x2<=0.5", cre_results_1[["CATE_results"]][,1])))
  expect_true(any(grepl("x2>0.5 & x3>0.5", cre_results_1[["CATE_results"]][,1])))

  # DRLearner CATE estimation with AIPW ITE estimation
  print("Testing DRLearner CATE estimation with AIPW ITE estimation")
  cre_results_2 <- cre(y, z, X, ratio_dis, ite_method_dis = "aipw",
                       include_ps_dis = NA, ps_method_dis = "SL.xgboost",
                       or_method_dis = "SL.xgboost", ite_method_inf = "aipw",
                       include_ps_inf = NA, ps_method_inf = "SL.xgboost",
                       or_method_inf = "SL.xgboost", ntrees_rf, ntrees_gbm,
                       min_nodes, max_nodes, t, q, rules_method,
                       include_offset, offset_name, cate_method = "DRLearner",
                       cate_SL_library = "SL.xgboost")
  expect_true(any(grepl("x1<=0.5 & x2<=0.5", cre_results_2[["CATE_results"]][,1])))
  expect_true(any(grepl("x2>0.5 & x3>0.5", cre_results_2[["CATE_results"]][,1])))

  # BART-baggr CATE estimation with BART ITE estimation
  print(paste("The code to test BART-baggr CATE estimation ",
              "with BART ITE estimation is commented out due to time. ",
              "Uncomment the code to run it."))
  #cre_results_3 <- cre(y, z, X, ratio_dis, ite_method_dis = "bart",
  #                     include_ps_dis = TRUE, ps_method_dis = "SL.xgboost",
  #                     or_method_dis = NA, ite_method_inf = "bart",
  #                     include_ps_inf = TRUE, ps_method_inf = "SL.xgboost",
  #                     or_method_inf = NA, ntrees_rf, ntrees_gbm, min_nodes,
  #                     max_nodes, t, q, rules_method, include_offset, offset_name,
  #                     cate_method = "bart-baggr", cate_SL_library = NA)
  #expect_true(any(grepl("x1<=0.5 & x2<=0.5", cre_results_3[["CATE_results"]][,1])))
  #expect_true(any(grepl("x2>0.5 & x3>0.5", cre_results_3[["CATE_results"]][,1])))

  # CF-means CATE estimation with BCF ITE estimation
  print("Testing CF-means CATE estimation with BCF ITE estimation")
  cre_results_4 <- cre(y, z, X, ratio_dis, ite_method_dis = "bcf",
                       include_ps_dis = NA, ps_method_dis = "SL.xgboost",
                       or_method_dis = NA, ite_method_inf = "bcf",
                       include_ps_inf = NA, ps_method_inf = "SL.xgboost",
                       or_method_inf = NA, ntrees_rf, ntrees_gbm, min_nodes,
                       max_nodes, t, q, rules_method, include_offset, offset_name,
                       cate_method = "cf-means", cate_SL_library = NA)
  expect_true(any(grepl("x1<=0.5 & x2<=0.5", cre_results_4[["CATE_results"]][,1])))
  expect_true(any(grepl("x2>0.5 & x3>0.5", cre_results_4[["CATE_results"]][,1])))

  # Linear Regression CATE estimation with BART ITE estimation
  print("Testing Linear Regression CATE estimation with BART ITE estimation")
  cre_results_5 <- cre(y, z, X, ratio_dis, ite_method_dis = "bart",
                       include_ps_dis = TRUE, ps_method_dis = "SL.xgboost",
                       or_method_dis = NA, ite_method_inf = "bart",
                       include_ps_inf = TRUE, ps_method_inf = "SL.xgboost",
                       or_method_inf = NA, ntrees_rf, ntrees_gbm, min_nodes,
                       max_nodes, t, q, rules_method, include_offset,
                       offset_name, cate_method = "linreg", cate_SL_library = NA)
  expect_true(any(grepl("x1<=0.5 & x2<=0.5", cre_results_5[["CATE_results"]][,1])))
  expect_true(any(grepl("x2>0.5 & x3>0.5", cre_results_5[["CATE_results"]][,1])))
})
