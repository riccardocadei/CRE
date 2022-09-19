#' @title
#' The Causal Rule Ensemble
#'
#' @description
#' Performs the Causal Rule Ensemble on a data set with a response variable,
#'  a treatment variable, and various features
#'
#' @param y The observed response vector.
#' @param z The treatment vector.
#' @param X The covariate matrix.
#' @param method_params parameters for individual treatment effect, includig:
#'   - *Parameters for Discovery*
#'     - *ratio_dis*: The ratio of data delegated to the discovery sub-sample.
#'     - *ite_method_dis*: The method to estimate the discovery sample ITE.
#'     - *include_ps_dis*: Whether or not to include propensity score estimate
#'       as a covariate in discovery ITE estimation, considered only for BART, XBART,
#'       or CF.
#'     - *ps_method_dis*: The estimation model for the propensity score on the
#'       discovery subsample.
#'     - *or_method_dis*: The estimation model for the outcome regressions
#'       estimate_ite_aipw on the discovery subsample.
#'   - *Parameters for Inference*
#'     - *ite_method_inf*: the method to estimate the inference sample ITE.
#'     - *include_ps_inf* whether or not to include propensity score estimate as
#'       a covariate in inference ITE estimation, considered only for BART, XBART,
#'       or CF.
#'     - *include_ps_inf*: whether or not to include propensity score estimate as
#'       a covariate in inference ITE estimation, considered only for BART, XBART,
#'       or CF.
#'     - *ps_method_inf*: the estimation model for the propensity score on the
#'       inference subsample.
#'     - *or_method_inf*: the estimation model for the outcome regressions in
#'       estimate_ite_aipw on the inference subsample.
#'   - *Other Parameters*:
#'     - *rules_method*: the method for selecting causal rules with binary outcomes.
#'     - *include_offset*: whether or not to include an offset when estimating
#'  the ITE, for poisson only.
#'     - *offset_name*: the name of the offset, if it is to be included.
#'     - *cate_method*: the method to estimate the CATE values.
#'     - *cate_SL_library*: the library used if cate_method is set to DRLearner.
#'     - *filter_cate*: whether or not to filter rules with p-value <= 0.05.
#' @param hyper_params the list of parameters required to tune the functions,
#' including:
#'  - *ntrees_rf*: the number of decision trees for randomForest.
#'  - *ntrees_gbm*: the number of decision trees for gradient boosting.
#'  - *min_nodes*: the minimum size of the trees' terminal nodes.
#'  - *max_nodes*: the maximum size of the trees' terminal nodes.
#'  - *t*: the common support used in generating the causal rules matrix.
#'  - *q*: the selection threshold used in selecting the causal rules.

#'
#' @return
#' An S3 object containing the matrix of Conditional
#'  Average Treatment Effect estimates
#'
#' @export
#'
cre <- function(y, z, X, method_params, hyper_params){

  # Input checks ---------------------------------------------------------------
  check_input_data(y = y, z = z, X = X)
  method_params <- check_method_params(y = y, params = method_params)
  check_hyper_params(params = hyper_params)

  # Unlist params into the current environment.
  # Interim approach.
  list2env(method_params, envir = environment())
  list2env(hyper_params, envir = environment())

  # Split data -----------------------------------------------------------------
  logger::log_info("Working on splitting data ... ")
  X_names <- names(as.data.frame(X))
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)
  subgroups <- split_data(y, z, X, getElement(method_params,"ratio_dis"))
  discovery <- subgroups[["discovery"]]
  inference <- subgroups[["inference"]]

  # Generate outcome (y), exposure(z), and covariate matrix (X) for discovery
  # and inference data
  y_dis <- discovery[,1]
  z_dis <- discovery[,2]
  X_dis <- discovery[,3:ncol(discovery)]

  y_inf <- inference[,1]
  z_inf <- inference[,2]
  X_inf <- inference[,3:ncol(inference)]

  # Discovery ------------------------------------------------------------------

  logger::log_info("Conducting Discovery Subsample Analysis ... ")

  # Estimate ITE -----------------------
  logger::log_info("Estimating ITE ... ")
  st_ite_t <- proc.time()
  ite_list_dis <- estimate_ite(y = y_dis, z = z_dis, X = X_dis,
                               ite_method = getElement(method_params,"ite_method_dis"),
                               is_y_binary = getElement(method_params,"is_y_binary"),
                               include_ps = getElement(method_params,"include_ps_dis"),
                               ps_method = getElement(method_params,"ps_method_dis"),
                               oreg_method = getElement(method_params,"oreg_method_dis"),
                               X_names = X_names,
                               include_offset = getElement(method_params,"include_offset"),
                               offset_name = getElement(method_params,"offset_name"),
                               random_state = getElement(method_params, "random_state"))
  en_ite_t <- proc.time()
  logger::log_debug("Finished Estimating ITE. ",
                    " Wall clock time: {(en_ite_t - st_ite_t)[[3]]} seconds.")

  ite_dis <- ite_list_dis[["ite"]]
  ite_std_dis <- ite_list_dis[["ite_std"]]

  # Generate rules list ----------------
  logger::log_info("Generating Initial Causal Rules ... ")
  initial_rules_dis <- generate_rules(X_dis, ite_std_dis,
                                      getElement(hyper_params,"ntrees_rf"),
                                      getElement(hyper_params,"ntrees_gbm"),
                                      getElement(hyper_params,"min_nodes"),
                                      getElement(hyper_params,"max_nodes"),
                                      getElement(method_params, "random_state"))

  # Generate rules matrix --------------
  logger::log_info("Generating Causal Rules Matrix ...")
  rules_all_dis <- generate_rules_matrix(X_dis, initial_rules_dis, t)
  rules_matrix_dis <- rules_all_dis[["rules_matrix"]]
  rules_matrix_std_dis <- rules_all_dis[["rules_matrix_std"]]
  rules_list_dis <- rules_all_dis[["rules_list"]]

  # Select important rules -------------
  logger::log_info("Selecting Important Causal Rules ...")
  select_rules_dis <- as.character(select_causal_rules(rules_matrix_std_dis,
                                                       rules_list_dis,
                                                       ite_std_dis,
                                                       getElement(method_params,"is_y_binary"),
                                                       getElement(hyper_params,"q"),
                                                       getElement(hyper_params,"rules_method")))

  select_rules_matrix_dis <- rules_matrix_dis[,which(rules_list_dis %in%
                                                       select_rules_dis)]
  select_rules_matrix_std_dis <- rules_matrix_std_dis[,which(rules_list_dis %in%
                                                             select_rules_dis)]
  if (length(select_rules_dis) == 0){
     stop("No significant rules were discovered. Ending Analysis.")
  }

  # Inference ------------------------------------------------------------------

  logger::log_info("Conducting Inference Subsample Analysis ...")
  message("Conducting Inference Subsample Analysis")
  ite_list_inf <- estimate_ite(y = y_inf, z = z_inf, X = X_inf,
                               ite_method = getElement(method_params,"ite_method_inf"),
                               is_y_binary = getElement(method_params,"is_y_binary"),
                               include_ps = getElement(method_params,"include_ps_inf"),
                               ps_method = getElement(method_params,"ps_method_inf"),
                               oreg_method = getElement(method_params,"oreg_method_inf"),
                               X_names = X_names,
                               include_offset = getElement(method_params,"include_offset"),
                               offset_name = getElement(method_params,"offset_name"),
                               random_state = getElement(method_params, "random_state"))

  ite_inf <- ite_list_inf[["ite"]]
  ite_std_inf <- ite_list_inf[["ite_std"]]
  sd_ite_inf <- ite_list_inf[["sd_ite"]]

  # Estimate CATE ----------------------
  logger::log_info("Estimating CATE ...")
  rules_matrix_inf <- matrix(0, nrow = dim(X_inf)[1],
                             ncol = length(select_rules_dis))
  for (i in 1:length(select_rules_dis)) {
    rules_matrix_inf[eval(parse(text = select_rules_dis[i]),
                          list(X = X_inf)), i] <- 1
  }
  select_rules_interpretable <- interpret_select_rules(select_rules_dis,
                                                       X_names)
  cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names,
                            getElement(method_params,"include_offset"),
                            getElement(method_params,"offset_name"),
                            rules_matrix_inf, select_rules_interpretable,
                            getElement(method_params,"cate_method"),
                            ite_inf, sd_ite_inf,
                            getElement(method_params,"cate_SL_library"),
                            getElement(method_params,"filter_cate"))

  # Convert cate_inf into an S3 object
  make_S3 <- function(cate_inf, cate_method) {
    S3_object <- list()
    S3_object[["CATE_results"]] <- cate_inf
    S3_object[["CATE_method"]] <- cate_method
    # item_names <- colnames(cate_inf)
    # for (i in 1:length(item_names)) {
    #   S3_object[[item_names[i]]] <- cate_inf[,i]
    # }
    attr(S3_object, "class") <- "cre"
    return(S3_object)
  }

  # Return Results
  logger::log_info("CRE method complete. Returning results.")
  cate_S3 <- make_S3(cate_inf, getElement(method_params,"cate_method"))
  return(cate_S3)
}
