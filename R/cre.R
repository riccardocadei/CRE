#' @title
#' Causal rule ensemble
#'
#' @description
#' Performs the Causal Rule Ensemble on a data set with a response variable,
#' a treatment variable, and various features.
#'
#' @param y An observed response vector.
#' @param z A treatment vector.
#' @param X A covariate matrix (or a data frame). Should be provided as
#' numerical values.
#' @param method_params The list of parameters to define the models used,
#' including:
#'   - *Parameters for Honest Splitting*
#'     - *ratio_dis*: The ratio of data delegated to rules discovery
#'     (default: 0.5).
#'   - *Parameters for Discovery and Inference*
#'     - *ite_method*: The method for ITE (pseudo-outcome) estimation
#'     (default: 'aipw', options: {'aipw' for Augmented Inverse Probability
#'     Weighting, 'cf' for Causal Forest, 'bart' for Causal Bayesian Additive
#'     Regression Trees, slearner' for S-Learner, 'tlearner' for T-Learner,
#'     'xlearner' for X-Learner, 'tpoisson' for T-Learner with Poisson
#'     regression}).
#'     - *learner_ps*: The model for the propensity score estimation
#'     (default: 'SL.xgboost', options: {any SuperLearner prediction model i.e.,
#'     'SL.lm', 'SL.svm'}, used only for 'aipw','bart','cf' ITE estimators).
#'     - *learner_y*: The model for the outcome estimation
#'     (default: 'SL.xgboost', options: {any SuperLearner prediction model i.e.,
#'     'SL.lm', 'SL.svm'}, used only for 'aipw','slearner','tlearner' and
#'     'xlearner' ITE estimators).
#' @param hyper_params The list of hyper parameters to fine-tune the method,
#' including:
#'  - *General hyper parameters*
#'    - *intervention_vars*: Array with intervention-able covariates names used
#'    for Rules Generation. Empty or null array means that all the covariates are
#'    considered as intervention-able (default: NULL).
#'    - *ntrees*: The number of decision trees for random forest (default: 20).
#'    - *node_size*: Minimum size of the trees' terminal nodes (default: 20).
#'    - *max_rules*: Maximum number of generated candidates rules (default: 50).
#'    - *max_depth*: Maximum rules length (default: 3).
#'    - *t_decay*: The decay threshold for rules pruning. Higher values will
#'    carry out an aggressive pruning (default: 0.025).
#'    - *t_ext*: The threshold to truncate too generic or too specific (extreme)
#'    rules (default: 0.01, range: [0, 0.5)).
#'    - *t_corr*: The threshold to define correlated rules (default: 1,
#'    range: [0,+inf]).
#'    - *stability_selection*: Method for stability selection for selecting the
#'    rules. `vanilla` for stability selection, `error_control`
#'    for stability selection with error control and `no` for no stability
#'    selection (default: `vanilla`).
#'    - *B*: Number of bootstrap samples for stability selection in rules
#'    selection and uncertainty quantification in estimation (default: 20).
#'    - *subsample*: Bootstrap ratio subsample for stability selection in rules
#'    selection and uncertainty quantification in estimation (default: 0.5).
#'  - *Method specific hyper parameters*
#'    - *offset*: Name of the covariate to use as offset (i.e. 'x1') for
#'    T-Poisson ITE estimation. Use NULL if offset is not used (default: NULL).
#'    - *cutoff*:  Threshold (percentage) defining the minimum cutoff value for
#'    the stability scores for Stability Selection (default: 0.9).
#'    - *pfer*: Upper bound for the per-family error rate (tolerated amount of
#'    falsely selected rules) for Error Control Stability Selection (default: 1).
#'
#' @param ite The estimated ITE vector. If given both the ITE estimation steps
#' in Discovery and Inference are skipped (default: NULL).
#'
#'
#' @return
#' An S3 object composed by:
#' \item{M}{the number of Decision Rules extracted at each step,}
#'  \item{CATE}{the data.frame of Conditional Average Treatment Effect
#'  decomposition estimates with corresponding uncertainty quantification,}
#'  \item{method_params}{the list of method parameters,}
#'  \item{hyper_params}{the list of hyper parameters,}
#'  \item{rules}{the list of rules (implicit form) decomposing the CATE.}
#'
#' @note
#' - If `intervention_vars` are provided, it's important to note that the
#' individual treatment effect will still be computed using all covariates.
#' @export
#'
#' @examples
#'
#' \donttest{
#' set.seed(123)
#' dataset <- generate_cre_dataset(n = 400,
#'                                 rho = 0,
#'                                 n_rules = 2,
#'                                 p = 10,
#'                                 effect_size = 2,
#'                                 binary_covariates = TRUE,
#'                                 binary_outcome = FALSE,
#'                                 confounding = "no")
#' y <- dataset[["y"]]
#' z <- dataset[["z"]]
#' X <- dataset[["X"]]
#'
#' method_params <- list(ratio_dis = 0.5,
#'                       ite_method ="aipw",
#'                       learner_ps = "SL.xgboost",
#'                       learner_y = "SL.xgboost")
#'
#' hyper_params <- list(intervention_vars = NULL,
#'                      offset = NULL,
#'                      ntrees = 20,
#'                      node_size = 20,
#'                      max_rules = 50,
#'                      max_depth = 3,
#'                      t_decay = 0.025,
#'                      t_ext = 0.025,
#'                      t_corr = 1,
#'                      stability_selection = "vanilla",
#'                      cutoff = 0.6,
#'                      pfer = 1,
#'                      B = 20,
#'                      subsample = 0.5)
#'
#' cre_results <- cre(y, z, X, method_params, hyper_params)
#'}
#'
cre <- function(y, z, X,
                method_params = NULL, hyper_params = NULL, ite = NULL) {

  "%>%" <- magrittr::"%>%"

  # timing the function
  st_time_cre <- proc.time()

  # Input checks ---------------------------------------------------------------
  check_input_data(y, z, X, ite)
  method_params <- check_method_params(y = y,
                                       ite = ite,
                                       params = method_params)
  hyper_params <- check_hyper_params(X_names = colnames(as.data.frame(X)),
                                     params = hyper_params)

  # Honest Splitting -----------------------------------------------------------
  subgroups <- honest_splitting(y, z, X,
                                getElement(method_params, "ratio_dis"), ite)
  discovery <- subgroups[["discovery"]]
  inference <- subgroups[["inference"]]

  y_dis <- discovery$y
  z_dis <- discovery$z
  X_dis <- discovery$X
  ite_dis <- discovery$ite

  y_inf <- inference$y
  z_inf <- inference$z
  X_inf <- inference$X
  ite_inf <- inference$ite

  intervention_vars <- getElement(hyper_params, "intervention_vars")

  # Discovery ------------------------------------------------------------------
  logger::log_info("Starting rules discovery...")
  st_time_rd <- proc.time()
  # Estimate ITE
  if (is.null(ite)) {
    ite_dis <- estimate_ite(y = y_dis,
                          z = z_dis,
                          X = X_dis,
                          ite_method = getElement(method_params, "ite_method"),
                          learner_ps = getElement(method_params, "learner_ps"),
                          learner_y = getElement(method_params, "learner_y"),
                          offset = getElement(method_params, "offset"))
  } else {
    logger::log_info("Using the provided ITE estimations...")
  }

  # Filter only Intervention-able variables
  if (!is.null(intervention_vars)) {
    X_dis <- X_dis[, intervention_vars, drop = FALSE]
  }

  # Discover Decision Rules
  discovery <- discover_rules(X_dis,
                              ite_dis,
                              method_params,
                              hyper_params)
  rules <- discovery[["rules"]]
  M <- discovery[["M"]]

  en_time_rd <- proc.time()
  logger::log_info("Done with rules discovery. ",
                   "(WC: {g_wc_str(st_time_rd, en_time_rd)}", ".)")
  # Inference ------------------------------------------------------------------
  logger::log_info("Starting inference...")
  st_time_inf <- proc.time()

  # Estimate ITE
  if (is.null(ite)) {
    ite_inf <- estimate_ite(y = y_inf,
                            z = z_inf,
                            X = X_inf,
                            ite_method = getElement(method_params, "ite_method"),
                            learner_ps = getElement(method_params, "learner_ps"),
                            learner_y = getElement(method_params, "learner_y"),
                            offset = getElement(method_params, "offset"))
  } else {
    logger::log_info("Skipped generating ITE.",
                     "The provided ITE will be used.")
  }

  # Filter only Intervention-able variables
  if (!is.null(intervention_vars)) {
    X_inf <- X_inf[, intervention_vars, drop = FALSE]
  }

  # Generate rules matrix
  if (length(rules) == 0) {
    rules_matrix_inf <- NA
    rules_explicit <- c()
  } else {
    rules_matrix_inf <- generate_rules_matrix(X_inf, rules)
    rules_explicit <- interpret_rules(rules, colnames(as.data.frame(X)))
  }

  # Estimate CATE
  cate_inf <- estimate_cate(rules_matrix_inf,
                            rules_explicit,
                            ite_inf,
                            getElement(hyper_params, "B"),
                            getElement(hyper_params, "subsample"))
  M["select_significant"] <- as.integer(length(cate_inf$Rule)) - 1

  # Estimate ITE
  if (M["select_significant"] > 0) {
    rules <- rules[rules_explicit %in% cate_inf$Rule[2:length(cate_inf$Rule)]]
    rules_explicit <- cate_inf$Rule[2:length(cate_inf$Rule)]
  } else {
    rules <- NULL
    rules_explicit <- NULL
  }

  en_time_inf <- proc.time()
  logger::log_info("Done with inference. ",
                   "(WC: {g_wc_str(st_time_inf, en_time_inf)} ", ".)")

  # Generate final results S3 object
  results <- list("M" = M,
                  "CATE" = cate_inf,
                  "method_params" = method_params,
                  "hyper_params" = hyper_params,
                  "rules" = rules)
  attr(results, "class") <- "cre"

  # Return Results -------------------------------------------------------------
  end_time_cre <- proc.time()
  logger::log_info("Done with running CRE function!",
                   "(WC: {g_wc_str(st_time_cre, end_time_cre)}", ".)")
  return(results)
}
