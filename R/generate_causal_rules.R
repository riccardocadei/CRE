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

  # Generate rules -------------------------------------------------------------
  logger::log_info("Generating Decision Rules...")
  rules <- generate_rules(X,
                          ite_std,
                          getElement(hyper_params,"intervention_vars"),
                          getElement(hyper_params,"ntrees_rf"),
                          getElement(hyper_params,"ntrees_gbm"),
                          getElement(hyper_params,"node_size"),
                          getElement(hyper_params,"max_nodes"),
                          getElement(hyper_params,"max_depth"),
                          getElement(hyper_params,"replace"))
  M_initial <- length(rules)

  # Filtering ------------------------------------------------------------------

  # Discard irrelevant variable-value pair from a rule condition ---------------
  logger::log_info("Filtering irrelevant rules...")
  rules_list <- filter_irrelevant_rules(rules, X, ite_std,
                                        getElement(hyper_params,"max_decay"),
                                        getElement(hyper_params,"type_decay"))
  M_filter1 <- length(rules_list)

  # Generate rules matrix ------------------------------------------------------
  rules_matrix <- generate_rules_matrix(X, rules_list)

  # Discard rules with too few or too many observations and correlated rules ---
  logger::log_info("Filtering extreme rules...")
  rules_matrix <- filter_extreme_rules(rules_matrix, rules_list,
                                       getElement(hyper_params,"t_ext"))
  rules_list <- colnames(rules_matrix)
  M_filter2 <- length(rules_list)

  # Discard correlated rules ---------------------------------------------------
  logger::log_info("Filtering correlated rules...")
  rules_matrix <- filter_correlated_rules(rules_matrix, rules_list,
                                           getElement(hyper_params,"t_corr"))
  rules_list <- colnames(rules_matrix)
  M_filter3 <- length(rules_list)

  # Discover Causal Rules ---------------------------------------------------
  logger::log_info("Discovering Causal Rules...")
  #rules_matrix_std <- standardize_rules_matrix(rules_matrix)
  rules_list <- as.character(discover_causal_rules(rules_matrix,
                                 rules_list,
                                 ite_std,
                                 getElement(hyper_params,"stability_selection"),
                                 getElement(hyper_params,"cutoff"),
                                 getElement(hyper_params,"pfer"),
                                 getElement(hyper_params,"penalty_rl")))
  M_filter4 <- length(rules_list)

  M <- list("Initial" = M_initial,
            "Filter 1 (irrelevant)" = M_filter1,
            "Filter 2 (extreme)" = M_filter2,
            "Filter 3 (correlated)" = M_filter3,
            "Causal" = M_filter4)

  return(list(rules=rules_list,M=M))
}

