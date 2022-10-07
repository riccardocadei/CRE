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
#' @param method_params Parameters for individual treatment effect, includig:
#'   - *Parameters for Discovery*
#'     - *ratio_dis*: The ratio of data delegated to the discovery sub-sample.
#'     - *ite_method_dis*: The method to estimate the discovery sample ITE.
#'     - *include_ps_dis*: Whether or not to include propensity score estimate
#'       as a covariate in discovery ITE estimation, considered only for BART,
#'       or CF.
#'     - *ps_method_dis*: The estimation model for the propensity score on the
#'       discovery subsample.
#'     - *or_method_dis*: The estimation model for the outcome regressions
#'       estimate_ite_aipw on the discovery subsample.
#'   - *Parameters for Inference*
#'     - *ite_method_inf*: The method to estimate the inference sample ITE.
#'     - *include_ps_inf*: Whether or not to include propensity score estimate
#'       as a covariate in inference ITE estimation, considered only for BART,
#'       or CF.
#'     - *include_ps_inf*: Whether or not to include propensity score estimate
#'       as a covariate in inference ITE estimation, considered only for BART,
#'       or CF.
#'     - *ps_method_inf*: The estimation model for the propensity score on the
#'       inference subsample.
#'     - *or_method_inf*: The estimation model for the outcome regressions in
#'       estimate_ite_aipw on the inference subsample.
#'   - *Other Parameters*:
#'     - *include_offset*: Whether or not to include an offset when estimating
#'  the ITE, for Poisson only.
#'     - *offset_name*: The name of the offset, if it is to be included.
#'     - *cate_method*: The method to estimate the CATE values.
#'     - *cate_SL_library*: The library used if cate_method is set to DRLearner.
#'     - *filter_cate*: Whether or not to filter rules with p-value <= 0.05.
#' @param hyper_params The list of parameters required to tune the functions,
#' including:
#'  - *effect_modifiers*: Effect Modifiers for Rules Generation.
#'  - *ntrees_rf*: The number of decision trees for randomForest.
#'  - *ntrees_gbm*: The number of decision trees for gradient boosting.
#'  - *node_size*: The minimum size of the trees' terminal nodes.
#'  - *max_nodes*: The maximum number of terminal nodes trees in the forest can
#'   have.
#'  - *max_depth*: The number of top levels from each tree considered
#' to extract conditions.
#'  - *max_decay*: Decay Threshold for pruning the rules.
#'  - *type_decay*: Decay Type for pruning the rules (1: relative error; 2: error).
#'  - *t*: The common support used in generating the causal rules matrix.
#'  - *q*: The selection threshold used in selecting the causal rules.
#'  - *stability_selection*: Whether or not using stability selection for
#'  selecting the causal rules.
#'  - *pfer_val*: The Per-Family Error Rate, the expected number of false
#'  discoveries.
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

  # Generate Causal Decision Rules  -----------------------
  select_rules_dis_list <- generate_causal_rules(X_dis, ite_std_dis, method_params, hyper_params)
  select_rules_dis <- select_rules_dis_list[["rules"]]
  M <- select_rules_dis_list[["M"]]
  M_final <- M[["Filter 3 (LASSO)"]]
  logger::log_info("{M_final} significant Causal Rules were discovered.")


  # Inference ------------------------------------------------------------------

  logger::log_info("Conducting Inference Subsample Analysis ...")
  message("Conducting Inference Subsample Analysis")

  # Estimate ITE -----------------------
  logger::log_info("Estimating ITE ... ")
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

  # Generate rules matrix --------------
  logger::log_info("Generating Causal Rules Matrix ...")
  rules_matrix_inf <- matrix(0,
                             nrow = dim(X_inf)[1],
                             ncol = M_final)
  if (M_final>0){
    for (i in 1:M_final) {
      rules_matrix_inf[eval(parse(text = select_rules_dis[i]),
                            list(X = X_inf)),
                       i] <- 1
    }
  }

  select_rules_interpretable <- interpret_select_rules(select_rules_dis, X_names)

  # Estimate CATE ----------------------
  logger::log_info("Estimating CATE ...")
  cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names,
                            getElement(method_params,"include_offset"),
                            getElement(method_params,"offset_name"),
                            rules_matrix_inf, select_rules_interpretable,
                            getElement(method_params,"cate_method"),
                            ite_inf, sd_ite_inf,
                            getElement(method_params,"cate_SL_library"),
                            getElement(method_params,"filter_cate"))

  # Generate final results S3 object
  results <- list()
  results[["M"]] <- M
  results[["CATE"]] <- cate_inf
  results[["cate_method"]] <- getElement(method_params,"cate_method")
  attr(results, "class") <- "cre"

  # Return Results
  logger::log_info("CRE method complete. Returning results.")

  return(results)
}
