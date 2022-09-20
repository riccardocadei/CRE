#' @examples
#' dataset <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
#'                                 effect_size = 2, binary = FALSE)
#'
#' # Initialize parameters
#' y <- dataset[["y"]]
#' z <- dataset[["z"]]
#' X <- as.data.frame(dataset[["X"]])
#' X_names <- names(as.data.frame(X))
#' ratio_dis <- 0.25
#' ite_method_dis <- "oreg"
#' include_ps_dis <- TRUE
#' ps_method_dis <- "SL.xgboost"
#' oreg_method_dis <- NA
#' ntrees_rf <- 100
#' ntrees_gbm <- 50
#' min_nodes <- 20
#' max_nodes <- 5
#' t <- 0.025
#' include_offset <- FALSE
#' offset_name <- NA
#' binary <- FALSE
#'
#' # Split data
#' X <- as.matrix(X)
#' y <- as.matrix(y)
#' z <- as.matrix(z)
#' subgroups <- CRE:::split_data(y, z, X, ratio_dis)
#' discovery <- subgroups[[1]]
#' inference <- subgroups[[2]]
#'
#' # Generate y, z, and X for discovery and inference data
#' y_dis <- discovery[,1]
#' z_dis <- discovery[,2]
#' X_dis <- discovery[,3:ncol(discovery)]
#'
#' # Estimate ITE on Discovery Subsample
#' ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis,
#'                              ite_method = ite_method_dis,
#'                              include_ps = include_ps_dis,
#'                              ps_method = ps_method_dis,
#'                              oreg_method = oreg_method_dis,
#'                              is_y_binary = binary,
#'                              X_names = X_names,
#'                              include_offset = include_offset,
#'                              offset_name = offset_name,
#'                              random_state = 121)
#'
#' ite_dis <- ite_list_dis[["ite"]]
#' ite_std_dis <- ite_list_dis[["ite_std"]]
#'
#' # Generate rules list
#' initial_rules_dis <- CRE:::generate_rules(X_dis, ite_std_dis, ntrees_rf,
#'                                           ntrees_gbm,
#'                                           min_nodes, max_nodes,
#'                                           random_state = 561)
#'
#' # Generate rules matrix
#' rules_all_dis <- CRE:::generate_rules_matrix(X_dis, initial_rules_dis, t)
#'
