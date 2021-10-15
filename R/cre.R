#' @title
#' The Causal Rule Ensemble
#'
#' @description
#' Performs the Causal Rule Ensemble on a dataset with a response variable,
#'  a treatment variable, and various features
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the covariate matrix
#' @param ratio_dis the ratio of data delegated to the discovery sub-sample
#' @param ite_method_dis the method to estimate the discovery sample ITE
#' @param include_ps_dis whether or not to include propensity score estimate
#'  as a covariate in discovery ITE estimation, considered only for BART, XBART,
#'   or CF
#' @param ite_method_inf the method to estimate the inference sample ITE
#' @param include_ps_inf whether or not to include propensity score estimate as
#'  a covariate in inference ITE estimation, considered only for BART, XBART,
#'   or CF
#' @param ntrees_rf the number of decision trees for randomForest
#' @param ntrees_gbm the number of decision trees for gradient boosting
#' @param min_nodes the minimum size of the trees' terminal nodes
#' @param max_nodes the maximum size of the trees' terminal nodes
#' @param t the common support used in generating the causal rules matrix
#' @param q the selection threshold used in selecting the causal rules
#' @param rules_method the method for selecting causal rules with binary outcomes
#' @param include_offset whether or not to include an offset when estimating
#'  the ITE, for poisson only
#' @param offset_name the name of the offset, if it is to be included
#'
#' @return
#' a list containing the list of select causal rules and a matrix of Conditional
#'  Average Treatment Effect estimates
#'
#' @export
#'
cre <- function(y, z, X, ratio_dis, ite_method_dis, include_ps_dis = NA,
                ite_method_inf,  include_ps_inf = NA,  ntrees_rf, ntrees_gbm,
                min_nodes, max_nodes, t, q, rules_method,
                include_offset = FALSE, offset_name = NA) {

  # Check for correct numerical inputs
  if (!(class(y) %in% c("numeric", "integer"))){
    stop("Invalid 'y' input. Please input a numeric vector.")
  }

  if (length(unique(z)) != 2){
    stop("Invalid 'z' input. Please input a binary treatment vector.")
  }

  if (length(class(X)) == 1) {
    if (!(class(X) %in% c("data.frame", "matrix"))) {
      stop("Invalid 'X' input. Please input a matrix or data frame.")
    }
  }
  if (length(class(X)) == 2) {
    if (!(identical(class(X), c("matrix", "array")))) {
      stop("Invalid 'X' input. Please input a matrix or data frame.")
    }
  }

  X_classes <- apply(X, 2, class)

  if (!all(X_classes %in% c("integer", "numeric"))){
    stop(paste("Invalid 'X' input. Please input a matrix or data frame",
               " of numeric categorical variables"))
  }

  if (class(ratio_dis) != "numeric" | !dplyr::between(ratio_dis, 0, 1)){
    stop("Invalid 'ratio_dis' input. Please input a number between 0 and 1.")
  }

  if (class(ntrees_rf) != "numeric"){
    stop("Invalid 'ntrees_rf' input. Please input a number.")
  }

  if (class(ntrees_gbm) != "numeric"){
    stop("Invalid 'ntrees_gbm' input. Please input a number.")
  }

  if (class(min_nodes) != "numeric"){
    stop("Invalid 'min_nodes' input. Please input a number.")
  }

  if (class(max_nodes) != "numeric"){
    stop("Invalid 'max_nodes' input. Please input a number.")
  }

  if (class(t) != "numeric"){
    stop("Invalid 't' input. Please input a number.")
  }

  if (class(q) != "numeric"){
    stop("Invalid 'q' input. Please input a number.")
  }

  # Check for correct ITE inputs
  ite_method_dis <- tolower(ite_method_dis)
  if (!(ite_method_dis %in% c("ipw", "sipw", "or", "bart", "xbart", "bcf",
                              "xbcf", "cf", "poisson"))) {
    stop("Invalid ITE method for Discovery Subsample. Please choose from the following:
         'ipw', 'sipw', 'or', 'bart', 'xbart', 'bcf', 'xbcf', 'cf', or 'poisson'")
  }

  ite_method_inf <- tolower(ite_method_inf)

  if (!(ite_method_inf %in% c("ipw", "sipw", "or", "bart", "xbart", "bcf",
                              "xbcf", "cf", "poisson"))) {
    stop("Invalid ITE method for Inference Subsample. Please choose from the following:
         'ipw', 'sipw', 'or', 'bart', 'xbart', 'bcf', 'xbcf', 'cf', or 'poisson'")
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
      stop(paste("The 'ipw', 'sipw', 'bcf', and 'xbcf' methods are not ",
                 "applicable to data with binary outcomes.Please select a ",
                 "method from the following: 'or', 'cf', 'bart', or 'xbart'"))
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

  # Check for correct offset input
  if (ite_method_dis == "poisson" | ite_method_inf == "poisson") {
    if (include_offset == TRUE) {
      if (is.na(offset_name)) {
        stop("Invalid offset_name input. Please specify an offset_name if you wish to include an offset.")
      }
    } else {
      offset_name <- NA
    }
  } else {
    include_offset <- FALSE
    offset_name <- NA
  }

  # Split data
  message("Splitting Data")
  X_names <- names(as.data.frame(X))
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

  ###### Discovery ######
  message("Conducting Discovery Subsample Analysis")

  # Estimate ITE
  message("Estimating ITE")
  ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis, ite_method_dis,
                               include_ps_dis, binary, X_names, include_offset,
                               offset_name)

  ite_dis <- ite_list_dis[["ite"]]
  ite_std_dis <- ite_list_dis[["ite_std"]]

  # Generate rules list
  message("Generating Initial Causal Rules")
  initial_rules_dis <- generate_rules(X_dis, ite_std_dis, ntrees_rf, ntrees_gbm,
                                      min_nodes, max_nodes)

  # Generate rules matrix
  message("Generating Causal Rules Matrix")
  rules_all_dis <- generate_rules_matrix(X_dis, initial_rules_dis, t)
  rules_matrix_dis <- rules_all_dis[["rules_matrix"]]
  rules_matrix_std_dis <- rules_all_dis[["rules_matrix_std"]]
  rules_list_dis <- rules_all_dis[["rules_list"]]

  # Select important rules
  message("Selecting Important Causal Rules")
  select_rules_dis <- as.character(select_causal_rules(rules_matrix_std_dis,
                                                       rules_list_dis,
                                                       ite_std_dis, binary, q,
                                                       rules_method))

  select_rules_matrix_dis <- rules_matrix_dis[,which(rules_list_dis %in% select_rules_dis)]
  select_rules_matrix_std_dis <- rules_matrix_std_dis[,which(rules_list_dis %in% select_rules_dis)]
  if (length(select_rules_dis) == 0) stop("No significant rules were discovered. Ending Analysis.")

  ###### Inference ######
  message("Conducting Inference Subsample Analysis")
  if (ite_method_inf != "poisson") {
    ite_list_inf <- estimate_ite(y_inf, z_inf, X_inf, ite_method_inf,
                                 include_ps_inf, binary, X_names,
                                 include_offset, offset_name)
    ite_inf <- ite_list_inf[["ite"]]
    ite_std_inf <- ite_list_inf[["ite_std"]]
    sd_ite_inf <- ite_list_inf[["sd_ite"]]
  } else {
    ite_inf <- NA
    ite_std_inf <- NA
    sd_ite_inf <- NA
  }

  # Estimate CATE
  message("Estimating CATE")
  rules_matrix_inf <- matrix(0, nrow = dim(X_inf)[1],
                             ncol = length(select_rules_dis))
  for (i in 1:length(select_rules_dis)) {
    rules_matrix_inf[eval(parse(text = select_rules_dis[i]),
                          list(X = X_inf)), i] <- 1
  }
  select_rules_interpretable <- interpret_select_rules(select_rules_dis, X_names)
  cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names,
                            include_offset, offset_name,
                            rules_matrix_inf, select_rules_interpretable,
                            ite_method_inf, ite_inf, sd_ite_inf)

  # Return Results
  message("CRE method complete. Returning results.")
  return(cate_inf)
}
