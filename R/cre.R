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
#' @param include_ps_dis whether or not to include propensity score estimate as a covariate in discovery ITE estimation, considered only for BART, XBART, or CF
#' @param include_ps_inf whether or not to include propensity score estimate as a covariate in inference ITE estimation, considered only for BART, XBART, or CF
#' @param ntrees_rf the number of decision trees for randomForest
#' @param ntrees_gbm the number of decision trees for gradient boosting
#' @param min_nodes the minimum size of the trees' terminal nodes
#' @param max_nodes the maximum size of the trees' terminal nodes
#' @param t the common support used in generating the causal rules matrix
#' @param q the selection threshold used in selecting the causal rules
#' @param rules_method the method for selecting causal rules with binary outcomes
#'
#' @return a list containing a select list of causal rules, Conditional Average Treatment Effect estimates, and a sensitivity analysis
#'
#' @export
#'
cre <- function(y, z, X, ratio_dis, ite_method_dis, ite_method_inf,
                include_ps_dis = NA, include_ps_inf = NA, ntrees_rf, ntrees_gbm,
                min_nodes, max_nodes, t, q, rules_method) {
  # Check for correct numerical inputs
  if (class(y) != "numeric") stop("Invalid 'y' input. Please input a numeric vector.")
  if (class(z) != "integer") stop("Invalid 'z' input. Please input a binary treatment vector.")
  if (length(class(X)) == 1) {
    if (class(X) != "data.frame") {
      stop("Invalid 'X' input. Please input a matrix or data frame.")
    }
  }
  if (length(class(X)) == 2) {
    if (!(identical(class(X), c("matrix", "array")))) {
      stop("Invalid 'X' input. Please input a matrix or data frame.")
    }
  }
  if (class(ratio_dis) != "numeric" | !dplyr::between(ratio_dis, 0, 1)) stop("Invalid 'ratio_dis' input. Please input a number between 0 and 1.")
  if (class(ntrees_rf) != "numeric") stop("Invalid 'ntrees_rf' input. Please input a number.")
  if (class(ntrees_gbm) != "numeric") stop("Invalid 'ntrees_gbm' input. Please input a number.")
  if (class(min_nodes) != "numeric") stop("Invalid 'min_nodes' input. Please input a number.")
  if (class(max_nodes) != "numeric") stop("Invalid 'max_nodes' input. Please input a number.")
  if (class(t) != "numeric") stop("Invalid 't' input. Please input a number.")
  if (class(q) != "numeric") stop("Invalid 'q' input. Please input a number.")

  # Check for correct ITE inputs
  ite_method_dis <- tolower(ite_method_dis)
  if (!(ite_method_dis %in% c("ipw", "sipw", "or", "bart", "xbart", "bcf", "xbcf", "cf"))) {
    stop("Invalid ITE method for Discovery Subsample. Please choose from the following:
         'ipw', 'sipw', or, 'bart', 'xbart', 'bcf', 'xbcf', or 'cf'")
  }
  ite_method_inf <- tolower(ite_method_inf)
  if (!(ite_method_inf %in% c("ipw", "sipw", "or", "bart", "xbart", "bcf", "xbcf", "cf"))) {
    stop("Invalid ITE method for Inference Subsample. Please choose from the following:
         'ipw', 'sipw', or, 'bart', 'xbart', 'bcf', 'xbcf', or 'cf'")
  }

  # Check for correct propensity score estimation inputs
  include_ps_dis <- toupper(include_ps_dis)
  if (ite_method_dis %in% c("bart", "xbart", "cf")) {
    if (!(include_ps_dis %in% c(TRUE, FALSE))) {
      stop("Please specify 'TRUE' or 'FALSE' for the include_ps_dis argument.")
    }
  } else {
    include_ps_dis <- NA
  }
  include_ps_inf <- toupper(include_ps_inf)
  if (ite_method_inf %in% c("bart", "xbart", "cf")) {
    if (!(include_ps_inf %in% c(TRUE, FALSE))) {
      stop("Please specify 'TRUE' or 'FALSE' for the include_ps_inf argument.")
    }
  } else {
    include_ps_inf <- NA
  }

  # Determine outcome type
  binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)
  if (binary) {
    if (ite_method_dis %in% c("bcf", "xbcf", "ipw", "sipw") |
        ite_method_inf %in% c("bcf", "xbcf", "ipw", "sipw")) {
      stop("The 'ipw', 'sipw', 'bcf', and 'xbcf' methods are not applicable to data with binary outcomes.
           Please select a method from the following: 'or', 'cf', 'bart', or 'xbart'")
    }
  }

  # Check for correct rules_method input
  rules_method <- tolower(rules_method)
  if (binary) {
    if (!(rules_method %in% c("conservative", "anticonservative"))) {
      stop("Invalid rules_method input. Please specify 'conservative' or 'anticonservative'.")
    }
  } else {
    rules_method <- NA
  }

  # Step 1: Split data
  message("Step 1: Splitting Data")
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
  X_inf <- inference[,3:ncol(discovery)]

  ###### Discovery ######
  message("Conducting Discovery Subsample Analysis")

  # Step 2: Estimate ITE
  message("Step 2: Estimating ITE")
  ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis, ite_method_dis, include_ps_dis, binary)
  ite_dis <- ite_list_dis[["ite"]]
  ite_std_dis <- ite_list_dis[["ite_std"]]

  # Step 3: Generate rules list
  message("Step 3: Generating Initial Causal Rules")
  initial_rules_dis <- generate_rules(X_dis, ite_std_dis, ntrees_rf, ntrees_gbm, min_nodes, max_nodes)

  # Step 4: Generate rules matrix
  message("Step 4: Generating Causal Rules Matrix")
  rules_all_dis <- generate_rules_matrix(X_dis, initial_rules_dis, t)
  rules_matrix_dis <- rules_all_dis[["rules_matrix"]]
  rules_matrix_std_dis <- rules_all_dis[["rules_matrix_std"]]
  rules_list_dis <- rules_all_dis[["rules_list"]]

  # Step 5: Select important rules
  message("Step 5: Selecting Important Causal Rules")
  select_rules_dis <- select_causal_rules(rules_matrix_std_dis, rules_list_dis,
                                          ite_std_dis, binary, q, rules_method)
  select_rules_matrix_dis <- rules_matrix_dis[,which(rules_list_dis %in% select_rules_dis)]
  select_rules_matrix_std_dis <- rules_matrix_std_dis[,which(rules_list_dis %in% select_rules_dis)]

  ###### Inference ######
  message("Conducting Inference Subsample Analysis")

  # Step 2: Estimate ITE
  message("Step 2: Estimating ITE")
  ite_list_inf <- estimate_ite(y_inf, z_inf, X_inf, ite_method_inf, include_ps_inf, binary)
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
