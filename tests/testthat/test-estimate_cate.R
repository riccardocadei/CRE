test_that("CATE (DRLearner) Estimation Runs Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- as.data.frame(dataset_cont[["X"]])
  X_names <- names(as.data.frame(X))
  ratio_dis <- 0.25
  ite_method_dis <- "bart"
  include_ps_dis <- "TRUE"
  ps_method_dis <- "SL.xgboost"
  oreg_method_dis <- NA
  ite_method_inf <- "bart"
  include_ps_inf <- "FALSE"
  ps_method_inf <- "SL.xgboost"
  oreg_method_inf <- "SL.xgboost"
  ntrees_rf <- 100
  ntrees_gbm <- 50
  node_size <- 20
  max_nodes <- 5
  max_depth <- 15
  replace <- FALSE
  max_decay <- 0.025
  type_decay <- 2
  t <- 0.025
  q <- 0.8
  pfer_val <- 0.1
  stability_selection <- TRUE
  include_offset <- FALSE
  offset_name <- NA
  cate_method <- "DRLearner"
  cate_SL_library <- "SL.xgboost"
  filter_cate <- FALSE

  # Check for binary outcome
  binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)

  # Step 1: Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)
  subgroups <- split_data(y, z, X, ratio_dis)
  discovery <- subgroups[[1]]
  inference <- subgroups[[2]]

  # Generate y, z, and X for discovery and inference data
  y_dis <- discovery[,1]
  z_dis <- discovery[,2]
  X_dis <- discovery[,3:ncol(discovery)]

  y_inf <- inference[,1]
  z_inf <- inference[,2]
  X_inf <- inference[,3:ncol(inference)]

  # Step 2: Estimate ITE
  ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis, ite_method_dis, binary,
                               include_ps = include_ps_dis,
                               ps_method = ps_method_dis,
                               oreg_method = oreg_method_dis,
                               X_names = X_names,
                               include_offset = include_offset,
                               offset_name = offset_name,
                               random_state = 112)
  ite_dis <- ite_list_dis[["ite"]]
  ite_std_dis <- ite_list_dis[["ite_std"]]

  # Step 3: Generate rules list
  initial_rules_dis <- generate_rules(X_dis, ite_std_dis, ntrees_rf, ntrees_gbm,
                                      node_size, max_nodes, max_depth, replace,
                                      random_state = 981)

  rules_list_dis <- prune_rules(initial_rules_dis, X_dis, ite_std_dis, max_decay, type_decay)

  # Step 4: Generate rules matrix
  rules_matrix_dis <- generate_rules_matrix(X_dis, rules_list_dis)
  rules_matrix_std_dis <- standardize_rules_matrix(rules_matrix_dis)

  # Step 5: Select important rules
  select_rules_dis <- as.character(lasso_rules_filter(rules_matrix_std_dis,
                                                       rules_list_dis,
                                                       ite_std_dis, q,
                                                       stability_selection, pfer_val))
  select_rules_matrix_dis <- rules_matrix_dis[,which(rules_list_dis %in%
                                                       select_rules_dis)]
  select_rules_matrix_std_dis <- rules_matrix_std_dis[,which(rules_list_dis %in%
                                                               select_rules_dis)]

  # Step 6: Estimate CATE
  ite_list_inf <- estimate_ite(y_inf, z_inf, X_inf, ite_method_inf, binary,
                               include_ps = include_ps_inf,
                               ps_method = ps_method_inf,
                               oreg_method = oreg_method_inf,
                               X_names = X_names,
                               include_offset = include_offset,
                               offset_name = offset_name,
                               random_state = 2568)
  ite_inf <- ite_list_inf[["ite"]]
  ite_std_inf <- ite_list_inf[["ite_std"]]
  sd_ite_inf <- ite_list_inf[["sd_ite"]]

  if (length(select_rules_dis)==0){
    rules_matrix_inf <- NA
    select_rules_interpretable <- NA
  } else {
    rules_matrix_inf <- generate_rules_matrix(X_inf, select_rules_dis)
    select_rules_interpretable <- interpret_select_rules(select_rules_dis, X_names)
  }

  ###### Run Tests ######

  # TO DO: add test to check wrong arguments

  # Correct outputs
  cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names, include_offset,
                            offset_name, rules_matrix_inf,
                            select_rules_interpretable, cate_method, ite_inf,
                            sd_ite_inf, cate_SL_library, filter_cate)
  expect_true(class(cate_inf) == "data.frame")
})


test_that("CATE (cf-means) Estimation Runs Correctly", {

  set.seed(99687)
  dataset <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                  effect_size = 2, binary = FALSE)
  # Initialize parameters
  y <- dataset[["y"]]
  z <- dataset[["z"]]
  X <- as.data.frame(dataset[["X"]])
  X_names <- names(as.data.frame(X))
  ratio_dis <- 0.25
  ite_method_dis <- "bart"
  include_ps_dis <- TRUE
  ps_method_dis <- "SL.xgboost"
  oreg_method_dis <- NA
  ite_method_inf <- "bart"
  include_ps_inf <- TRUE
  ps_method_inf <- "SL.xgboost"
  oreg_method_inf <- NA
  ntrees_rf <- 100
  ntrees_gbm <- 50
  node_size <- 20
  max_nodes <- 5
  max_depth <- 15
  replace <- FALSE
  max_decay <- 0.025
  type_decay <- 2
  t <- 0.025
  q <- 0.8
  pfer_val <- 0.1
  stability_selection <- TRUE
  include_offset <- FALSE
  offset_name <- NA
  binary <- FALSE
  cate_method <- "cf-means"
  cate_SL_library <- "SL.xgboost"
  filter_cate <- FALSE
  # Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)
  subgroups <- CRE:::split_data(y, z, X, ratio_dis)
  discovery <- subgroups[[1]]
  inference <- subgroups[[2]]
  # Generate y, z, and X for discovery and inference data
  y_dis <- discovery[,1]
  z_dis <- discovery[,2]
  X_dis <- discovery[,3:ncol(discovery)]
  y_inf <- inference[,1]
  z_inf <- inference[,2]
  X_inf <- inference[,3:ncol(inference)]
  # Estimate ITE on Discovery Subsample
  ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis,
                               ite_method =ite_method_dis,
                               include_ps = include_ps_dis,
                               ps_method = ps_method_dis,
                               oreg_method = oreg_method_dis,
                               is_y_binary = binary,
                               X_names = X_names,
                               include_offset = include_offset,
                               offset_name = offset_name,
                               random_state = 234)
  ite_dis <- ite_list_dis[["ite"]]
  ite_std_dis <- ite_list_dis[["ite_std"]]

  # Generate rules list
  initial_rules_dis <- CRE:::generate_rules(X_dis, ite_std_dis, ntrees_rf,
                                            ntrees_gbm, node_size, max_nodes,
                                            max_depth, replace, random_state = 214)

  rules_list_dis <- CRE:::prune_rules(initial_rules_dis, X_dis, ite_std_dis, max_decay, type_decay)

  # Generate rules matrix
  rules_matrix_dis <- CRE:::generate_rules_matrix(X_dis, rules_list_dis)
  rules_matrix_std_dis <- CRE:::standardize_rules_matrix(rules_matrix_dis)

  # Select important rules
  select_rules_dis <- as.character(CRE:::lasso_rules_filter(rules_matrix_std_dis, rules_list_dis,
                                                             ite_std_dis, q, stability_selection,
                                                             pfer_val))
  select_rules_matrix_dis <- rules_matrix_dis[,which(rules_list_dis %in% select_rules_dis)]
  select_rules_matrix_std_dis <- rules_matrix_std_dis[,which(rules_list_dis %in% select_rules_dis)]

  # Step 6: Estimate CATE
  ite_list_inf <- estimate_ite(y_inf, z_inf, X_inf, ite_method_inf, binary,
                               include_ps = include_ps_inf,
                               ps_method = ps_method_inf,
                               oreg_method = oreg_method_inf,
                               X_names = X_names,
                               include_offset = include_offset,
                               offset_name = offset_name,
                               random_state = 2568)
  ite_inf <- ite_list_inf[["ite"]]
  ite_std_inf <- ite_list_inf[["ite_std"]]
  sd_ite_inf <- ite_list_inf[["sd_ite"]]

  if (length(select_rules_dis)==0){
    rules_matrix_inf <- NA
    select_rules_interpretable <- NA
  } else {
    rules_matrix_inf <- generate_rules_matrix(X_inf, select_rules_dis)
    select_rules_interpretable <- interpret_select_rules(select_rules_dis, X_names)
  }

  ###### Run Tests ######

  # TO DO: add test to check wrong arguments

  # Correct outputs
  cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names, include_offset,
                            offset_name, rules_matrix_inf,
                            select_rules_interpretable, cate_method, ite_inf,
                            sd_ite_inf, cate_SL_library, filter_cate)
  expect_true(class(cate_inf) == "data.frame")
})


test_that("CATE (linreg) Estimation Runs Correctly", {

  set.seed(99687)
  dataset <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                  effect_size = 2, binary = FALSE)
  # Initialize parameters
  y <- dataset[["y"]]
  z <- dataset[["z"]]
  X <- as.data.frame(dataset[["X"]])
  X_names <- names(as.data.frame(X))
  ratio_dis <- 0.25
  ite_method_dis <- "bart"
  include_ps_dis <- TRUE
  ps_method_dis <- "SL.xgboost"
  oreg_method_dis <- NA
  ite_method_inf <- "bart"
  include_ps_inf <- TRUE
  ps_method_inf <- "SL.xgboost"
  oreg_method_inf <- NA
  ntrees_rf <- 100
  ntrees_gbm <- 50
  node_size <- 20
  max_nodes <- 5
  max_depth <- 15
  replace <- FALSE
  max_decay <- 0.025
  type_decay <- 2
  t <- 0.025
  q <- 0.8
  pfer_val <- 0.1
  stability_selection <- TRUE
  include_offset <- FALSE
  offset_name <- NA
  binary <- FALSE
  cate_method <- "linreg"
  cate_SL_library <- "SL.xgboost"
  filter_cate <- FALSE
  # Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)
  subgroups <- CRE:::split_data(y, z, X, ratio_dis)
  discovery <- subgroups[[1]]
  inference <- subgroups[[2]]
  # Generate y, z, and X for discovery and inference data
  y_dis <- discovery[,1]
  z_dis <- discovery[,2]
  X_dis <- discovery[,3:ncol(discovery)]
  y_inf <- inference[,1]
  z_inf <- inference[,2]
  X_inf <- inference[,3:ncol(inference)]
  # Estimate ITE on Discovery Subsample
  ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis,
                               ite_method =ite_method_dis,
                               include_ps = include_ps_dis,
                               ps_method = ps_method_dis,
                               oreg_method = oreg_method_dis,
                               is_y_binary = binary,
                               X_names = X_names,
                               include_offset = include_offset,
                               offset_name = offset_name,
                               random_state = 234)
  ite_dis <- ite_list_dis[["ite"]]
  ite_std_dis <- ite_list_dis[["ite_std"]]

  # Generate rules list
  initial_rules_dis <- CRE:::generate_rules(X_dis, ite_std_dis, ntrees_rf,
                                            ntrees_gbm, node_size, max_nodes,
                                            max_depth, replace, random_state = 214)

  rules_list_dis <- CRE:::prune_rules(initial_rules_dis, X_dis, ite_std_dis, max_decay, type_decay)

  # Generate rules matrix
  rules_matrix_dis <- CRE:::generate_rules_matrix(X_dis, rules_list_dis)
  rules_matrix_std_dis <- CRE:::standardize_rules_matrix(rules_matrix_dis)

  # Select important rules
  select_rules_dis <- as.character(CRE:::lasso_rules_filter(rules_matrix_std_dis, rules_list_dis,
                                                            ite_std_dis, q, stability_selection,
                                                            pfer_val))
  select_rules_matrix_dis <- rules_matrix_dis[,which(rules_list_dis %in% select_rules_dis)]
  select_rules_matrix_std_dis <- rules_matrix_std_dis[,which(rules_list_dis %in% select_rules_dis)]

  # Step 6: Estimate CATE
  ite_list_inf <- estimate_ite(y_inf, z_inf, X_inf, ite_method_inf, binary,
                               include_ps = include_ps_inf,
                               ps_method = ps_method_inf,
                               oreg_method = oreg_method_inf,
                               X_names = X_names,
                               include_offset = include_offset,
                               offset_name = offset_name,
                               random_state = 2568)
  ite_inf <- ite_list_inf[["ite"]]
  ite_std_inf <- ite_list_inf[["ite_std"]]
  sd_ite_inf <- ite_list_inf[["sd_ite"]]

  if (length(select_rules_dis)==0){
    rules_matrix_inf <- NA
    select_rules_interpretable <- NA
  } else {
    rules_matrix_inf <- generate_rules_matrix(X_inf, select_rules_dis)
    select_rules_interpretable <- interpret_select_rules(select_rules_dis, X_names)
  }

  # TO DO: add test to check wrong arguments

  # Correct outputs
  cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names, include_offset,
                            offset_name, rules_matrix_inf,
                            select_rules_interpretable, cate_method, ite_inf,
                            sd_ite_inf, cate_SL_library, filter_cate)
  expect_true(class(cate_inf) == "data.frame")
})


test_that("CATE (linreg) Estimation Runs Correctly", {

  set.seed(99687)
  dataset <- generate_cre_dataset(n = 200, rho = 0, n_rules = 2, p = 10,
                                  effect_size = 2, binary = FALSE)
  # Initialize parameters
  y <- dataset[["y"]]
  z <- dataset[["z"]]
  X <- as.data.frame(dataset[["X"]])
  X_names <- names(as.data.frame(X))
  ratio_dis <- 0.25
  ite_method_dis <- "bart"
  include_ps_dis <- TRUE
  ps_method_dis <- "SL.xgboost"
  oreg_method_dis <- NA
  ite_method_inf <- "bart"
  include_ps_inf <- TRUE
  ps_method_inf <- "SL.xgboost"
  oreg_method_inf <- NA
  ntrees_rf <- 100
  ntrees_gbm <- 50
  node_size <- 20
  max_nodes <- 5
  max_depth <- 15
  replace <- FALSE
  max_decay <- 0.025
  type_decay <- 2
  t <- 0.025
  q <- 0.8
  pfer_val <- 0.1
  stability_selection <- TRUE
  include_offset <- FALSE
  offset_name <- NA
  binary <- FALSE
  cate_method <- "linreg"
  cate_SL_library <- "SL.xgboost"
  filter_cate <- FALSE
  # Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)
  subgroups <- CRE:::split_data(y, z, X, ratio_dis)
  discovery <- subgroups[[1]]
  inference <- subgroups[[2]]
  # Generate y, z, and X for discovery and inference data
  y_dis <- discovery[,1]
  z_dis <- discovery[,2]
  X_dis <- discovery[,3:ncol(discovery)]
  y_inf <- inference[,1]
  z_inf <- inference[,2]
  X_inf <- inference[,3:ncol(inference)]
  # Estimate ITE on Discovery Subsample
  ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis,
                               ite_method =ite_method_dis,
                               include_ps = include_ps_dis,
                               ps_method = ps_method_dis,
                               oreg_method = oreg_method_dis,
                               is_y_binary = binary,
                               X_names = X_names,
                               include_offset = include_offset,
                               offset_name = offset_name,
                               random_state = 234)
  ite_dis <- ite_list_dis[["ite"]]
  ite_std_dis <- ite_list_dis[["ite_std"]]

  # Generate rules list
  initial_rules_dis <- CRE:::generate_rules(X_dis, ite_std_dis, ntrees_rf,
                                            ntrees_gbm, node_size, max_nodes,
                                            max_depth, replace, random_state = 214)

  rules_list_dis <- CRE:::prune_rules(initial_rules_dis, X_dis, ite_std_dis, max_decay, type_decay)

  # Generate rules matrix
  rules_matrix_dis <- CRE:::generate_rules_matrix(X_dis, rules_list_dis)
  rules_matrix_std_dis <- CRE:::standardize_rules_matrix(rules_matrix_dis)

  # Select important rules
  select_rules_dis <- as.character(CRE:::lasso_rules_filter(rules_matrix_std_dis, rules_list_dis,
                                                            ite_std_dis, q, stability_selection,
                                                            pfer_val))
  select_rules_matrix_dis <- rules_matrix_dis[,which(rules_list_dis %in% select_rules_dis)]
  select_rules_matrix_std_dis <- rules_matrix_std_dis[,which(rules_list_dis %in% select_rules_dis)]

  # Step 6: Estimate CATE
  ite_list_inf <- estimate_ite(y_inf, z_inf, X_inf, ite_method_inf, binary,
                               include_ps = include_ps_inf,
                               ps_method = ps_method_inf,
                               oreg_method = oreg_method_inf,
                               X_names = X_names,
                               include_offset = include_offset,
                               offset_name = offset_name,
                               random_state = 2568)
  ite_inf <- ite_list_inf[["ite"]]
  ite_std_inf <- ite_list_inf[["ite_std"]]
  sd_ite_inf <- ite_list_inf[["sd_ite"]]

  if (length(select_rules_dis)==0){
    rules_matrix_inf <- NA
    select_rules_interpretable <- NA
  } else {
    rules_matrix_inf <- generate_rules_matrix(X_inf, select_rules_dis)
    select_rules_interpretable <- interpret_select_rules(select_rules_dis, X_names)
  }

  # TO DO: add test to check wrong arguments

  # Correct outputs
  cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names, include_offset,
                            offset_name, rules_matrix_inf,
                            select_rules_interpretable, cate_method, ite_inf,
                            sd_ite_inf, cate_SL_library, filter_cate)
  expect_true(class(cate_inf) == "data.frame")
})



test_that("CATE (bart-baggr) Estimation Runs Correctly", {

  set.seed(99687)
  dataset <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                  effect_size = 2, binary = FALSE)
  # Initialize parameters
  y <- dataset[["y"]]
  z <- dataset[["z"]]
  X <- as.data.frame(dataset[["X"]])
  X_names <- names(as.data.frame(X))
  ratio_dis <- 0.25
  ite_method_dis <- "bart"
  include_ps_dis <- TRUE
  ps_method_dis <- "SL.xgboost"
  oreg_method_dis <- NA
  ite_method_inf <- "bart"
  include_ps_inf <- TRUE
  ps_method_inf <- "SL.xgboost"
  oreg_method_inf <- NA
  ntrees_rf <- 100
  ntrees_gbm <- 50
  node_size <- 20
  max_nodes <- 5
  max_depth <- 15
  replace <- FALSE
  max_decay <- 0.025
  type_decay <- 2
  t <- 0.025
  q <- 0.8
  pfer_val <- 0.1
  stability_selection <- TRUE
  include_offset <- FALSE
  offset_name <- NA
  binary <- FALSE
  cate_method <- "bart-baggr"
  cate_SL_library <- "SL.xgboost"
  filter_cate <- FALSE
  # Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)
  subgroups <- CRE:::split_data(y, z, X, ratio_dis)
  discovery <- subgroups[[1]]
  inference <- subgroups[[2]]
  # Generate y, z, and X for discovery and inference data
  y_dis <- discovery[,1]
  z_dis <- discovery[,2]
  X_dis <- discovery[,3:ncol(discovery)]
  y_inf <- inference[,1]
  z_inf <- inference[,2]
  X_inf <- inference[,3:ncol(inference)]
  # Estimate ITE on Discovery Subsample
  ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis,
                               ite_method =ite_method_dis,
                               include_ps = include_ps_dis,
                               ps_method = ps_method_dis,
                               oreg_method = oreg_method_dis,
                               is_y_binary = binary,
                               X_names = X_names,
                               include_offset = include_offset,
                               offset_name = offset_name,
                               random_state = 234)
  ite_dis <- ite_list_dis[["ite"]]
  ite_std_dis <- ite_list_dis[["ite_std"]]

  # Generate rules list
  initial_rules_dis <- CRE:::generate_rules(X_dis, ite_std_dis, ntrees_rf,
                                            ntrees_gbm, node_size, max_nodes,
                                            max_depth, replace, random_state = 214)

  rules_list_dis <- CRE:::prune_rules(initial_rules_dis, X_dis, ite_std_dis, max_decay, type_decay)

  # Generate rules matrix
  rules_matrix_dis <- CRE:::generate_rules_matrix(X_dis, rules_list_dis)
  rules_matrix_std_dis <- CRE:::standardize_rules_matrix(rules_matrix_dis)

  # Select important rules
  select_rules_dis <- as.character(CRE:::lasso_rules_filter(rules_matrix_std_dis, rules_list_dis,
                                                            ite_std_dis, q, stability_selection,
                                                            pfer_val))
  select_rules_matrix_dis <- rules_matrix_dis[,which(rules_list_dis %in% select_rules_dis)]
  select_rules_matrix_std_dis <- rules_matrix_std_dis[,which(rules_list_dis %in% select_rules_dis)]

  # Step 6: Estimate CATE
  ite_list_inf <- estimate_ite(y_inf, z_inf, X_inf, ite_method_inf, binary,
                               include_ps = include_ps_inf,
                               ps_method = ps_method_inf,
                               oreg_method = oreg_method_inf,
                               X_names = X_names,
                               include_offset = include_offset,
                               offset_name = offset_name,
                               random_state = 2568)
  ite_inf <- ite_list_inf[["ite"]]
  ite_std_inf <- ite_list_inf[["ite_std"]]
  sd_ite_inf <- ite_list_inf[["sd_ite"]]

  if (length(select_rules_dis)==0){
    rules_matrix_inf <- NA
    select_rules_interpretable <- NA
  } else {
    rules_matrix_inf <- generate_rules_matrix(X_inf, select_rules_dis)
    select_rules_interpretable <- interpret_select_rules(select_rules_dis, X_names)
  }

  ###### Run Tests ######

  # TO DO: add test to check wrong arguments

  # Correct outputs
  cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names, include_offset,
                            offset_name, rules_matrix_inf,
                            select_rules_interpretable, cate_method, ite_inf,
                            sd_ite_inf, cate_SL_library, filter_cate)
  expect_true(class(cate_inf) == "data.frame")
})




