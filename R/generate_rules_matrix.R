#' @title
#' Generate Rules Matrix
#'
#' @description
#' Generates a matrix of causal rules given a list.
#'
#' @param X the features matrix
#' @param rules_list a vector of causal rules
#' @param t the common support used in generating the causal rules matrix
#'
#' @return
#' a list with:
#' - a raw matrix of causal rules
#' - a standardized matrix of causal rules, and
#' - a vector of causal rules
#'
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
generate_rules_matrix <- function(X, rules_list, t) {

  rules_matrix <- matrix(0, nrow = dim(X)[1], ncol = length(rules_list))

  for (i in 1:length(rules_list)){
    rules_matrix[eval(parse(text = rules_list[i])), i] <- 1
  }

  # Identify rules with too few observations
  nrules <- dim(rules_matrix)[2]
  ind <- 1:nrules
  if (dim(X)[1] < 200){
    t <- 0.05
  }

  sup <- apply(rules_matrix, 2, mean)
  elim <- which((sup < t) | (sup > (1 - t)))

  if (length(elim) > 0) {
    ind <- ind[-elim]
  }

  # Identify correlated rules
  corelim <- 1
  C <- stats::cor(rules_matrix[,ind])
  diag(C) <- 0
  nrules <- dim(rules_matrix[, ind])[2]
  elim <- c()

  for(i in 1:(nrules - 1)) {
    elim <- c(elim, which(round(abs(C[i, (i + 1):nrules]),
                                digits = 4) >= corelim) + i)
  }

  if (length(elim) > 0) {
    ind <- ind[-elim]
  } else {
    ind <- ind
  }

  # Remove rules with too few observations and correlated rules
  rules_matrix <- rules_matrix[, ind]
  rules_list <- rules_list[ind]

  # Standardize rules matrix
  mu_rules_matrix <- apply(rules_matrix, 2, mean)
  sd_rules_matrix <- apply(rules_matrix, 2, stats::sd)
  rules_matrix_std <- matrix(0, dim(X)[1], dim(rules_matrix)[2])
  for(l in 1:ncol(rules_matrix_std)){
    rules_matrix_std[, l] <- ((rules_matrix[, l] - mu_rules_matrix[l]) /
                                sd_rules_matrix[l])
  }

  return(list(rules_matrix = rules_matrix, rules_matrix_std = rules_matrix_std,
              rules_list = rules_list))
}
