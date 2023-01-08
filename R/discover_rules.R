#' @title
#' Discover Rules
#'
#' @description
#' Discover the minimal set of Rules Linearly Decomposing the Conditional
#' Average Treatment Effect (CATE).
#'
#' @param X A covariate matrix.
#' @param ite An estimated ITE.
#' @param method_params A vector of method parameters.
#' @param hyper_params A vector of hyper parameters.
#'
#' @return
#' The minimal set of Rules Linearly Decomposing the CATE.
#'
#' @keywords internal
#'
discover_rules <- function(X, ite, method_params, hyper_params) {

  # Generate rules -------------------------------------------------------------
  logger::log_info("Generating (candidate) Rules...")
  rules <- generate_rules(X,
                          ite,
                          getElement(hyper_params, "intervention_vars"),
                          getElement(hyper_params, "ntrees_rf"),
                          getElement(hyper_params, "ntrees_gbm"),
                          getElement(hyper_params, "node_size"),
                          getElement(hyper_params, "max_nodes"),
                          getElement(hyper_params, "max_depth"),
                          getElement(hyper_params, "replace"))
  M_initial <- length(rules)

  # Filtering ------------------------------------------------------------------

  # Discard irrelevant variable-value pair from a rule condition ---------------
  logger::log_info("Filtering Irrelevant Rules...")
  rules_list <- filter_irrelevant_rules(rules, X, ite,
                                        getElement(hyper_params, "max_decay"),
                                        getElement(hyper_params, "type_decay"))
  M_filter1 <- length(rules_list)

  # Generate rules matrix ------------------------------------------------------
  rules_matrix <- generate_rules_matrix(X, rules_list)

  # Discard rules with too few or too many observations and correlated rules ---
  logger::log_info("Filtering Extreme Rules...")
  rules_matrix <- filter_extreme_rules(rules_matrix, rules_list,
                                       getElement(hyper_params, "t_ext"))
  rules_list <- colnames(rules_matrix)
  M_filter2 <- length(rules_list)

  # Discard correlated rules ---------------------------------------------------
  logger::log_info("Filtering Correlated Rules...")
  rules_matrix <- filter_correlated_rules(rules_matrix, rules_list,
                                           getElement(hyper_params, "t_corr"))
  rules_list <- colnames(rules_matrix)
  M_filter3 <- length(rules_list)

  # Select Rules ---------------------------------------------------
  logger::log_info("Selecting Rules...")
  rules_list <- as.character(select_rules(rules_matrix,
                               rules_list,
                               ite,
                               getElement(hyper_params, "stability_selection"),
                               getElement(hyper_params, "cutoff"),
                               getElement(hyper_params, "pfer"),
                               getElement(hyper_params, "penalty_rl")))
  M_select1 <- length(rules_list)

  M <- list("Initial" = M_initial,
            "Filter (irrelevant)" = M_filter1,
            "Filter (extreme)" = M_filter2,
            "Filter (correlated)" = M_filter3,
            "Select (LASSO)" = M_select1)

  return(list(rules = rules_list, M = M))
}
