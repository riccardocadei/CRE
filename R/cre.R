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
#'     - *ps_method_inf*: The estimation model for the propensity score on the
#'       inference subsample.
#'     - *or_method_inf*: The estimation model for the outcome regressions in
#'       estimate_ite_aipw on the inference subsample.
#'   - *Other Parameters*
#'     - *include_offset*: Whether to include an offset (i.e. model outcome
#'       rate) or not (i.e. model outcome counts) for Poisson ITE Estimation
#'     - *offset_name*: The name of the covariate to use as offset (i.e. 'x1')
#'     - *cate_method*: The method to estimate the CATE values.
#'     - *cate_SL_library*: The library used if cate_method is set to DRLearner.
#'
#' @param hyper_params The list of parameters required to tune the functions,
#' including:
#'  - *intervention_vars*: Intervention-able variables used for Rules Generation.
#'  - *ntrees_rf*: The number of decision trees for randomForest.
#'  - *ntrees_gbm*: The number of decision trees for gradient boosting.
#'  - *node_size*: The minimum size of the trees' terminal nodes.
#'  - *max_nodes*: The maximum number of terminal nodes trees in the forest can
#'   have.
#'  - *max_depth*: The number of top levels from each tree considered
#' to extract conditions.
#'  - *replace*: Boolean variable for replacement in bootstrapping.
#'  - *max_decay*: Decay Threshold for pruning the rules.
#'  - *type_decay*: Decay Type for pruning the rules
#'  (1: relative error; 2: error).
#'  - *t_ext*: The threshold to define too generic or too specific (extreme)
#'  rules.
#'  - *t_corr*: The threshold to define correlated rules.
#'  - *t_pvalue*: the threshold to define statistically significant rules
#' (filter only causal decision rules with p-value <= t_pvalue).
#'  - *stability_selection*: Whether or not using stability selection for
#'  selecting the causal rules.
#'  - *cutoff*:  Threshold defining the minimum cutoff value for the stability
#' scores.
#'  - *pfer*: Upper bound for the per-family error rate (tolerated amount of
#' falsely selected rules).
#'  - *penalty_rl*: Order of penalty for rules length during LASSO for Causal
#' Rules Discovery (i.e. 0: no penalty, 1: ∝rules_length, 2: ∝rules_length^2)
#'
#' @return
#' An S3 object containing the matrix of Conditional
#'  Average Treatment Effect estimates
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#' set.seed(2021)
#' dataset <- generate_cre_dataset(n = 300, rho = 0, n_rules = 2, p = 10,
#'                                 effect_size = 2, binary_covariates = TRUE,
#'                                 binary_outcome = FALSE)
#' y <- dataset[["y"]]
#' z <- dataset[["z"]]
#' X <- dataset[["X"]]
#'
#' method_params <- list(ratio_dis = 0.25,
#'                       ite_method_dis="aipw",
#'                       ps_method_dis = "SL.xgboost",
#'                       oreg_method_dis = "SL.xgboost",
#'                       include_ps_dis = TRUE,
#'                       ite_method_inf = "aipw",
#'                       ps_method_inf = "SL.xgboost",
#'                       oreg_method_inf = "SL.xgboost",
#'                       include_ps_inf = TRUE,
#'                       include_offset = FALSE,
#'                       cate_method = "linreg",
#'                       cate_SL_library = "SL.xgboost",
#'                       offset_name = NA,
#'                       random_state = 3591)
#'
#' hyper_params <- list(ntrees_rf = 100,
#'                      ntrees_gbm = 50,
#'                      node_size = 20,
#'                      max_nodes = 5,
#'                      max_depth = 15,
#'                      max_decay = 0.025,
#'                      type_decay = 2,
#'                      t_ext = 0.025,
#'                      t_corr = 1,
#'                      t_pvalue = 0.05,
#'                      replace = FALSE,
#'                      stability_selection = TRUE,
#'                      cutoff = 0.6,
#'                      pfer = 0.1,
#'                      penalty_rl = 1)
#'
#' cre_results <- cre(y, z, X, method_params, hyper_params)
#'}
cre <- function(y, z, X, method_params, hyper_params){

  # Input checks ---------------------------------------------------------------
  logger::log_info("Loading dataset...")
  check_input_data(y = y, z = z, X = X)
  method_params <- check_method_params(y = y, params = method_params)
  check_hyper_params(params = hyper_params)

  # Honest Splitting -----------------------------------------------------------
  logger::log_info("(Honest) Splitting the dataset...")
  X_names <- names(as.data.frame(X))
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)
  subgroups <- honest_splitting(y, z, X, getElement(method_params,"ratio_dis"))
  discovery <- subgroups[["discovery"]]
  inference <- subgroups[["inference"]]

  # Generate outcome (y), exposure(z), and covariate matrix (X) for discovery
  # and inference data
  y_dis <- discovery$y
  z_dis <- discovery$z
  X_dis <- discovery$X

  y_inf <- inference$y
  z_inf <- inference$z
  X_inf <- inference$X


  # Discovery ------------------------------------------------------------------

  logger::log_info("Starting Causal Rules Discovery")

  # Estimate ITE -----------------------
  logger::log_info("Estimating ITE...")
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
  causal_rules_discovery <- generate_causal_rules(X_dis,
                                                  ite_std_dis,
                                                  method_params,
                                                  hyper_params)
  causal_rules <- causal_rules_discovery[["rules"]]
  M <- causal_rules_discovery[["M"]]



  # Inference ------------------------------------------------------------------

  logger::log_info("Starting CATE Inference")
  #message("Conducting Inference Subsample Analysis")

  # Estimate ITE ---------------------------------------------------------------
  logger::log_info("Estimating ITE...")
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

  # Generate rules matrix ------------------------------------------------------
  if (length(causal_rules)==0){
    rules_matrix_inf <- NA
    causal_rules_int <- NA
  } else {
    rules_matrix_inf <- generate_rules_matrix(X_inf, causal_rules)
    causal_rules_int <- interpret_select_rules(causal_rules, X_names)
  }

  # Estimate CATE --------------------------------------------------------------
  logger::log_info("Estimating CATE...")
  cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names,
                            getElement(method_params,"include_offset"),
                            getElement(method_params,"offset_name"),
                            rules_matrix_inf, causal_rules_int,
                            getElement(method_params,"cate_method"),
                            ite_inf, sd_ite_inf,
                            getElement(method_params,"cate_SL_library"),
                            getElement(hyper_params,"t_pvalue"))

  M["Causal (significant)"] <- as.integer(length(cate_inf$summary$Rule))-1

  # Estimate ITE----------------------------------------------------------------
  if (getElement(method_params,"cate_method")=="linreg"){
    if (!any(is.na(causal_rules_int))){
      causal_rules_matrix <- generate_rules_matrix(X, causal_rules)
      causal_rules_matrix <- as.data.frame(causal_rules_matrix) %>%
        dplyr::transmute_all(as.factor)
      names(causal_rules_matrix) <- causal_rules_int
      ite_pred <- predict(cate_inf$model, causal_rules_matrix)
    } else{
      ite_pred <- cate_inf$summary$Estimate[1]
    }
  } else {
    # TODO: return predicted ITEs for other CATE Estimators (i.e. DRLearner,...)
    ite_pred <- NULL
  }


  # Performance Evaluation------------------------------------------------------

  # Generate final results S3 object
  results <- list("M" = M,
                  "CATE" = cate_inf[["summary"]],
                  "cate_method" = getElement(method_params,"cate_method"),
                  "model" = cate_inf[["model"]],
                  "ite_pred" = ite_pred)
  attr(results, "class") <- "cre"

  # Sensitivity Analysis -------------------------------------------------------
  #logger::log_info("3 Sensitivity Analysis")
  # TO DO

  # Return Results -------------------------------------------------------------
  logger::log_info("CRE method complete")
  return(results)
}
