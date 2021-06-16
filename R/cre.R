#' @title
#' The Causal Rule Ensemble
#'
#' @description
#' Method for performing the Causal Rule Ensemble on a dataset with a response variable, a treatment variable, and various features
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param ratio_dis the ratio of data delegated to the discovery subsample
#' @param ite_method_dis the method to estimate the discovery sample ITE
#' @param ite_method_inf the method to estimate the inference sample ITE
#' @param ntrees the number of decision trees
#' @param min_nodes the minimum size of the trees' terminal nodes
#' @param max_nodes the maximum size of the trees' terminal nodes
#' @param rules_method the method for performing penalized regression on the causal rules to select only those that are important
#' @param t the common support used in generating the causal rules matrix
#'
#' @export
#'
#' @return a list containing a select list of causal rules, Conditional Average Treatment Effect estimates, and a sensitivity analysis
#'
cre <- function(y, z, X, ratio_dis, ite_method_dis, ite_method_inf, ntrees, min_nodes, max_nodes, rules_method, t) {
  # Step 1: Split data
  message("Step 1: Splitting Data")
  if (!("matrix" %in% class(X))) {
    X <- as.matrix(X)
  }
  subgroups <- CRE::split_data(y, z, X, ratio_dis)
  discovery <- subgroups[[1]]
  inference <- subgroups[[2]]

  # Generate y, z, and X for discovery and inference data
  y_dis <- discovery[,1]
  z_dis <- discovery[,2]
  X_dis <- discovery[,3:ncol(discovery)]

  y_inf <- inference[,1]
  z_inf <- inference[,2]
  X_inf <- inference[,3:ncol(discovery)]

  ###### Discovery ######
  message("Conducting Discovery Subsample Analysis")

  # Step 2: Estimate ITE
  message("Step 2: Estimating ITE")
  ite_list_dis <- CRE::estimate_ite(y_dis, z_dis, X_dis, ite_method_dis)
  ite_dis <- ite_list_dis[["ite"]]
  ite_std_dis <- ite_list_dis[["ite_std"]]

  # Step 3: Generate rules list
  message("Step 3: Generating Initial Causal Rules")
  initial_rules_dis <- CRE::generate_rules(X_dis, ite_std_dis, ntrees, min_nodes, max_nodes)

  # Step 4: Generate rules matrix
  message("Step 4: Generating Causal Rules Matrix")
  rules_all_dis <- CRE::generate_rules_matrix(X_dis, initial_rules_dis, t)
  rules_matrix_dis <- rules_all_dis[["rules_matrix"]]
  rules_matrix_std_dis <- rules_all_dis[["rules_matrix_std"]]
  rules_list_dis <- rules_all_dis[["rules_list"]]

  # Step 5: Select important rules
  message("Step 5: Selecting Important Causal Rules")
  select_rules_dis <- CRE::select_causal_rules(rules_matrix_std_dis, rules_list_dis, ite_std_dis, rules_method)
  select_rules_matrix_dis <- rules_matrix_dis[,which(rules_list_dis %in% select_rules_dis)]
  select_rules_matrix_std_dis <- rules_matrix_std_dis[,which(rules_list_dis %in% select_rules_dis)]

  ###### Inference ######
  message("Conducting Inference Subsample Analysis")

  # Step 2: Estimate ITE
  message("Step 2: Estimating ITE")
  ite_list_inf <- CRE::estimate_ite(y_inf, z_inf, X_inf, ite_method_inf)
  ite_inf <- ite_list_inf[["ite"]]
  ite_std_inf <- ite_list_inf[["ite_std"]]

  # Step 6: Estimate CATE
  message("Step 6: Estimating CATE")
  rules_all_inf <- CRE::generate_rules_matrix(X_inf, select_rules_dis, t)
  rules_list_inf <- rules_all_inf[["rules_list"]]
  rules_matrix_inf <- rules_all_inf[["rules_matrix"]]
  rules_matrix_std_inf <- rules_all_inf[["rules_matrix_std"]]
  cate_inf <- CRE::estimate_cate(ite_std_inf, rules_matrix_std_inf)

  # Step 7: Conduct sensitivity analysis
  message("Step 7: Conducting Sensitivity Analysis")
  # sensitivity_results <- analyze_sensitivity(ite_std_inf, rules_matrix_std_inf)

  # Return Results
  message("CRE method complete. Returning results.")
  cre_results <- list(select_rules = rules_list_inf, cate = cate_inf)
  return(cre_results)
}
