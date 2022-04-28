#' @title
#' The Causal Rule Ensemble
#'
#' @description
#' Performs the Causal Rule Ensemble on a dataset with a response variable,
#'  a treatment variable, and various features
#'
#' @param y The observed response vector.
#' @param z The treatment vector.
#' @param X The covariate matrix.
#' @param params the list of parameters required to run the function, including:
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
#'
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
#'   - *Other Parameters*
#'     - *ntrees_rf*: the number of decision trees for randomForest.
#'     - *ntrees_gbm*: the number of decision trees for gradient boosting.
#'     - *min_nodes*: the minimum size of the trees' terminal nodes.
#'     - *max_nodes*: the maximum size of the trees' terminal nodes.
#'     - *t*: the common support used in generating the causal rules matrix.
#'     - *q*: the selection threshold used in selecting the causal rules.
#'     - *rules_method*: the method for selecting causal rules with binary outcomes.
#'     - *include_offset*: whether or not to include an offset when estimating
#'  the ITE, for poisson only.
#'     - *offset_name*: the name of the offset, if it is to be included.
#'     - *cate_method*: the method to estimate the CATE values.
#'     - *cate_SL_library*: the library used if cate_method is set to DRLearner.
#'     - *filter_cate*: whether or not to filter rules with p-value <= 0.05.
#'
#' @return
#' an S3 object containing the matrix of Conditional
#'  Average Treatment Effect estimates
#'
#' @export
#'
#' @examples
#' dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
#'                                 effect_size = 2, binary = FALSE)
#'
#' cre_results <- cre(y = dataset[["y"]],
#'                    z = dataset[["z"]],
#'                    X = as.data.frame(dataset[["X"]]),
#'                    params = list(ratio_dis = 0.25,
#'                    ite_method_dis="bart",
#'                    include_ps_dis = TRUE,
#'                    ps_method_dis = "SL.xgboost",
#'                    ps_method_inf = "SL.xgboost",
#'                    ite_method_inf = "bart",
#'                    include_ps_inf = TRUE,
#'                    include_offset = FALSE,
#'                    cate_method = "DRLearner",
#'                    cate_SL_library = "SL.xgboost",
#'                    filter_cate = FALSE,
#'                    offset_name = NA,
#'                    ntrees_rf = 100,
#'                    ntrees_gbm = 50,
#'                    min_nodes = 20,
#'                    max_nodes = 5,
#'                    t = 0.025,
#'                    q = 0.8))
#'
cre <- function(y, z, X, params){

  # Input checks ---------------------------------------------------------------
  params <- check_input(y = y, z = z, X = X, params = params)

  # Unlist params into the current environment.
  list2env(params, envir = environment())

  # Split data -----------------------------------------------------------------
  logger::log_info("Working on splitting data ... ")
  X_names <- names(as.data.frame(X))
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)
  subgroups <- split_data(y, z, X, ratio_dis)
  discovery <- subgroups[[1]]
  inference <- subgroups[[2]]

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
  ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis, ite_method_dis,
                               include_ps_dis, ps_method_dis, or_method_dis,
                               binary, X_names, include_offset, offset_name)
  en_ite_t <- proc.time()
  logger::log_debug("Finished Estimating ITE. ",
                    " Wall clock time: {(en_ite_t - st_ite_t)[[3]]} seconds.")

  ite_dis <- ite_list_dis[["ite"]]
  ite_std_dis <- ite_list_dis[["ite_std"]]

  # Generate rules list ----------------
  logger::log_info("Generating Initial Causal Rules ... ")
  initial_rules_dis <- generate_rules(X_dis, ite_std_dis, ntrees_rf, ntrees_gbm,
                                      min_nodes, max_nodes)

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
                                                       ite_std_dis, binary, q,
                                                       rules_method))

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
  ite_list_inf <- estimate_ite(y_inf, z_inf, X_inf, ite_method_inf,
                               include_ps_inf, ps_method_inf, or_method_inf,
                               binary, X_names, include_offset, offset_name)
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
                            include_offset, offset_name,
                            rules_matrix_inf, select_rules_interpretable,
                            cate_method, ite_inf, sd_ite_inf,
                            cate_SL_library, filter_cate)

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
  cate_S3 <- make_S3(cate_inf, cate_method)
  return(cate_S3)
}
