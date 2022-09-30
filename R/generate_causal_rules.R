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
#' @export
#'
generate_causal_rules <- function(X, ite_std, method_params, hyper_params) {

  # Filter only Effect Modifiers -------
  effect_modifiers = getElement(hyper_params,"effect_modifiers")
  if (!is.null(effect_modifiers)) X <- X[,effect_modifiers]



  # Generate rules list ----------------
  logger::log_info("Rules generation ... ")
  initial_rules <- generate_rules(X, ite_std,
                                      getElement(hyper_params,"ntrees_rf"),
                                      getElement(hyper_params,"ntrees_gbm"),
                                      getElement(hyper_params,"node_size"),
                                      getElement(hyper_params,"max_nodes"),
                                      getElement(method_params,"random_state"))

  # Generate rules matrix --------------
  logger::log_info("Generating Causal Rules Matrix ...")
  rules_all <- generate_rules_matrix(X, initial_rules, t)
  rules_matrix <- rules_all[["rules_matrix"]]
  rules_matrix_std <- rules_all[["rules_matrix_std"]]
  rules_list <- rules_all[["rules_list"]]


  # Select important rules -------------
  logger::log_info("Rules Regularization ...")
  select_rules <- as.character(select_causal_rules(rules_matrix_std,
                                                       rules_list,
                                                       ite_std,
                                                       getElement(hyper_params,"q"),
                                                       getElement(hyper_params,"stability_selection"),
                                                       getElement(hyper_params,"pfer_val")
                                                       )
                                   )
  return(select_rules)
}

