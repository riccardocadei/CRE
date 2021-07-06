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
#' @param include_ps_dis whether or not to include propensity score estimate as a covariate in discovery ITE estimation
#' @param include_ps_inf whether or not to include propensity score estimate as a covariate in inference ITE estimation
#' @param ntrees the number of decision trees
#' @param min_nodes the minimum size of the trees' terminal nodes
#' @param max_nodes the maximum size of the trees' terminal nodes
#' @param t the common support used in generating the causal rules matrix
#'
#' @export
#'
#' @return a list containing a select list of causal rules, Conditional Average Treatment Effect estimates, and a sensitivity analysis
#'
cre <- function(y, z, X, ratio_dis, ite_method_dis, ite_method_inf, include_ps_dis, include_ps_inf, ntrees, min_nodes, max_nodes, t) {
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)

  # Check for binary outcome
  binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)
  if (ite_method_dis == "BCF" | ite_method_inf == "BCF") {
    stop("The Bayesian Causal Forest method does not apply to binary outcomes. Please select another method to estimate the ITE.")
  }

  # Check for propensity score estimation
  stopifnot(include_ps_dis %in% c("TRUE", "FALSE") & include_ps_inf %in% c("TRUE", "FALSE"))

  # Step 1: Split data
  message("Step 1: Splitting Data")
  subgroups <- split_data(y, z, X, ratio_dis)
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
  ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis, ite_method_dis, include_ps_dis)
  ite_dis <- ite_list_dis[["ite"]]
  ite_std_dis <- ite_list_dis[["ite_std"]]

  # Step 3: Generate rules list
  message("Step 3: Generating Initial Causal Rules")
  initial_rules_dis <- generate_rules(X_dis, ite_std_dis, ntrees, min_nodes, max_nodes)

  # Step 4: Generate rules matrix
  message("Step 4: Generating Causal Rules Matrix")
  rules_all_dis <- generate_rules_matrix(X_dis, initial_rules_dis, t)
  rules_matrix_dis <- rules_all_dis[["rules_matrix"]]
  rules_matrix_std_dis <- rules_all_dis[["rules_matrix_std"]]
  rules_list_dis <- rules_all_dis[["rules_list"]]

  # Step 5: Select important rules
  message("Step 5: Selecting Important Causal Rules")
  select_rules_dis <- select_causal_rules(rules_matrix_std_dis, rules_list_dis, ite_std_dis, binary)
  select_rules_matrix_dis <- rules_matrix_dis[,which(rules_list_dis %in% select_rules_dis)]
  select_rules_matrix_std_dis <- rules_matrix_std_dis[,which(rules_list_dis %in% select_rules_dis)]

  ###### Inference ######
  message("Conducting Inference Subsample Analysis")

  # Step 2: Estimate ITE
  message("Step 2: Estimating ITE")
  ite_list_inf <- estimate_ite(y_inf, z_inf, X_inf, ite_method_inf, include_ps_inf)
  ite_inf <- ite_list_inf[["ite"]]
  ite_std_inf <- ite_list_inf[["ite_std"]]

  # Step 6: Estimate CATE
  message("Step 6: Estimating CATE")
  rules_all_inf <- generate_rules_matrix(X_inf, select_rules_dis, t)
  rules_list_inf <- rules_all_inf[["rules_list"]]
  rules_matrix_inf <- rules_all_inf[["rules_matrix"]]
  rules_matrix_std_inf <- rules_all_inf[["rules_matrix_std"]]
  cate_inf <- estimate_cate(ite_inf, rules_matrix_inf, rules_list_inf)

  # Step 7: Conduct sensitivity analysis
  #message("Step 7: Conducting Sensitivity Analysis")
  #sensitivity_results <- analyze_sensitivity(ite_std_inf, rules_matrix_std_inf)

  # Return Results
  message("CRE method complete. Returning results.")
  cre_results <- list(select_rules = rules_list_inf, cate_means = cate_inf[[1]], cate_reg = cate_inf[[2]])
  return(cre_results)
}
