#' @title
#' Generate Causal Rules
#'
#' @description
#' Method for generating Causal Decision Rules.
#'
#' @param X The covariate matrix.
#' @param ite_std The standardized ITE.
#' @param method_params Method Parameters.
#' @param hyper_params Hyper Parameters.
#'
#' @return
#' A vector of Causal Rules.
#'
#' @keywords internal
#'
generate_causal_rules <- function(X, ite_std, method_params, hyper_params) {

  # Filter only Effect Modifiers -------
  effect_modifiers = getElement(hyper_params,"effect_modifiers")
  if (!is.null(effect_modifiers)) X <- X[,effect_modifiers,drop=FALSE]

  # 1. Generate rules --------------------
  logger::log_info("Rules generation ... ")
  rules <- generate_rules(X,
                          ite_std,
                          getElement(hyper_params,"ntrees_rf"),
                          getElement(hyper_params,"ntrees_gbm"),
                          getElement(hyper_params,"node_size"),
                          getElement(hyper_params,"max_nodes"),
                          getElement(hyper_params,"max_depth"),
                          getElement(hyper_params,"replace"),
                          getElement(method_params,"random_state"))
  M_initial <- length(rules)

  # 2. Select important rules -------------
  logger::log_info("Rules Regularization ...")

  # 2.1 Prune irrelevant variable-value pair from a rule condition
  logger::log_info("Pruning ...")
  rules_list <- prune_rules(rules,
                             X,
                             ite_std,
                             getElement(hyper_params,"max_decay"),
                             getElement(hyper_params,"type_decay"))
  M_filter1 <- length(rules_list)

  # Generate rules matrix
  logger::log_info("Generate Rules Matrix ...")
  rules_matrix <- generate_rules_matrix(X, rules_list)

  # 2.2 Remove rules with too few or too many observations and correlated rules
  logger::log_info("Remove anomalous rules ...")
  anomalous_temp <- discard_anomalous_rules(rules_matrix, rules_list,
                                            getElement(hyper_params,"t_anom"))
  rules_matrix <- anomalous_temp[["rules_matrix"]]
  rules_list <- anomalous_temp[["rules_list"]]
  M_filter2 <- length(rules_list)

  # 2.3 Remove correlated rules
  logger::log_info("Remove correlated rules ...")
  correlated_temp <- discard_correlated_rules(rules_matrix, rules_list,
                                              getElement(hyper_params,"t_corr"))
  rules_matrix <- correlated_temp[["rules_matrix"]]
  rules_list <- correlated_temp[["rules_list"]]
  M_filter3 <- length(rules_list)

  # Standardize rules matrix
  logger::log_info("Standardize Rules Matrix ...")
  rules_matrix_std <- standardize_rules_matrix(rules_matrix)

  # 2.4 LASSO
  logger::log_info("LASSO ...")
  rules_list <- as.character(lasso_rules_filter(rules_matrix_std,
                                                 rules_list,
                                                 ite_std,
                                                 getElement(hyper_params,"q"),
                                                 getElement(hyper_params,"stability_selection"),
                                                 getElement(hyper_params,"pfer_val")))
  M_filter4 <- length(rules_list)

  M <- list("Initial" = M_initial,
         "Filter 1 (pruning)" = M_filter1,
         "Filter 2 (anomalous)" = M_filter2,
         "Filter 3 (correlated)" = M_filter3,
         "Filter 4 (LASSO)" = M_filter4)

  return(list(rules=rules_list,M=M))
}

