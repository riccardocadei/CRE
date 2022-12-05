set.seed(2021)

# useful functions
{metrics <- function(ground_truth, prediction){
  intersect = intersect(prediction, ground_truth)
  union = union(prediction, ground_truth)
  TP = length(intersect)
  FP = length(setdiff(prediction, ground_truth))
  FN = length(setdiff(ground_truth, prediction))
  recall = TP/(TP+FN) # quantity
  precision = TP/(TP+FP) # quality
  IoU = length(intersect) / length(union)
  metrics <- list(recall = recall,
                  precision = precision,
                  IoU = IoU)
  return(metrics)
}

extract_effect_modifiers <- function(rules_list, X_names) {
  effect_modifiers <- c()
  for (X_name in X_names) {
    if (any(grepl(X_name,rules_pred))){
      effect_modifiers <- append(effect_modifiers, X_name)
    }
  }
  return(effect_modifiers)
}

generate_syn_dataset <- function(n = 1000, rho = 0, n_rules = 2, p = 10,
                                 effect_size = 2, binary = TRUE) {

  # Generate Covariate Matrix
  mu <- rep(0, p)
  Sigma <- matrix(rho, nrow = p, ncol = p) + diag(p) * (1 - rho)
  rawvars <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
  pvars <- stats::pnorm(rawvars)
  X <- stats::qbinom(pvars, 1, 0.5)
  colnames(X) <- paste("x", 1:p, sep = "")
  X <- as.data.frame(X)

  # Generate Treatment Vector
  logit.prob <- -1 + X$x1 - X$x2 + X$x3
  prob <- exp(logit.prob) / (1 + exp(logit.prob))
  z <- stats::rbinom(n, 1, prob = prob)

  # Generate Causal Rules and Potential Outcomes
  stopifnot(n_rules %in% c(2, 4))
  if (binary == TRUE){
    y0 <- rep(0, n)
    y1 <- rep(0, n)
    effect_size = 1
  }
  else {
    y0 <- stats::rnorm(n, mean = 0, sd = 1)
    y1 <- y0
  }
  y0[X$x1 == 1 & X$x2 == 0] = effect_size
  y1[X$x5 == 1 & X$x6 == 0] = effect_size
  if (n_rules == 4) {
    y0[X$x4 == 1] = effect_size
    y1[X$x5 == 0 & X$x7 == 1 & X$x8 == 0] = effect_size
  }

  # Generate Outcome
  y <- y0 * (1 - z) + y1 * z

  # Generate Observed Data
  dataset <- list(y = y, z = z, X = X)
  names(dataset) <- c("y", "z", "X")
  return(dataset)
}}

# Set Up Parameters
method_params <- list(ratio_dis = 0.5,
                      ite_method_dis="aipw",
                      ps_method_dis = "SL.xgboost",
                      oreg_method_dis = "SL.xgboost",
                      include_ps_dis = TRUE,
                      ite_method_inf = "aipw",
                      ps_method_inf = "SL.xgboost",
                      oreg_method_inf = "SL.xgboost",
                      include_ps_inf = TRUE,
                      include_offset = FALSE,
                      cate_method = "linreg",
                      cate_SL_library = "SL.xgboost",
                      random_state = 3591)

hyper_params <- list(intervention_vars = c(),
                     ntrees_rf = 100,
                     ntrees_gbm = 50,
                     node_size = 20,
                     max_nodes = 5,
                     max_depth = 15,
                     max_decay = 0,
                     type_decay = 2,
                     t_ext = 0.025,
                     t_corr = 1,
                     t_pvalue = 0.05,
                     replace = TRUE,
                     stability_selection = TRUE,
                     cutoff = 0.9,
                     pfer = 0.05,
                     penalty_rl = 1)

# Example
{dataset <- generate_syn_dataset(n = 1000, rho = 0,  p = 10, effect_size = 2,
                                 n_rules = 2, binary = FALSE)
  y <- dataset[["y"]]
  z <- dataset[["z"]]
  X <- dataset[["X"]]
  X_names <- colnames(X)

  cre_result <- cre(y, z, X, method_params, hyper_params)
  summary(cre_result, method_params, hyper_params)
  plot(cre_result)}


# Ground Truth
rules <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5","x4>0.5","x5<=0.5 & x7>0.5 & x8<=0.5")
effect_modifiers <- c("x1","x2","x5","x6","x4","x7","x8")

exp_names <- c("LASSO w0", "LASSO w1", "LASSO w2")
dataset_names <- c("C2","B2","C4","B4")

for (j in 1:length(exp_names)){
  if (grepl("w0", exp_names[j], fixed=TRUE)) {hyper_params['penalty_rl'] <- 0}
  else if (grepl("w1", exp_names[j], fixed=TRUE)) {hyper_params['penalty_rl'] <- 1}
  else if (grepl("w2", exp_names[j], fixed=TRUE)) {hyper_params['penalty_rl'] <- 2}
  for (i in 1:length(dataset_names)){
    if (grepl("B", dataset_names[i], fixed=TRUE)) {binary=TRUE}
    else if (grepl("C", dataset_names[i], fixed=TRUE)) {binary=FALSE}
    if (grepl("2", dataset_names[i], fixed=TRUE)) {n_rules=2}
    else if (grepl("4", dataset_names[i], fixed=TRUE)) {n_rules=4}
    dataset <- generate_syn_dataset(n = 3000, rho = 0,  p = 10, effect_size = 10,
                                    n_rules = n_rules,
                                    binary = binary)
    y <- dataset[["y"]]
    z <- dataset[["z"]]
    X <- dataset[["X"]]
    X_names <- colnames(X)

    cre_result <- cre(y, z, X, method_params, hyper_params)
    summary(cre_result, method_params, hyper_params)
    #plot(cre_result)
    rules_pred <- cre_result$CATE$Rule[cre_result$CATE$Rule %in% "(ATE)" == FALSE]
    effect_modifiers_pred <- extract_effect_modifiers(rules_list, X_names)
    if (n_rules==2){
      rules_gt = rules[1:2]
      effect_modifiers_gt = effect_modifiers[1:4]
    } else {
      rules_gt = rules
      effect_modifiers_gt = effect_modifiers
    }
    metrics_em <- metrics(effect_modifiers_gt,effect_modifiers_pred)
    metrics_rules <- metrics(rules_gt,rules_pred)
    result = list(result = cre_result,
                  rules = rules_pred,
                  effect_modifiers = effect_modifiers_pred,
                  metrics_em = metrics_em,
                  metrics_rules = metrics_rules)
    assign(dataset_names[i], result)
  }
  experiment = list("param" = c(method_params, hyper_params),
                    "B2" = B2,
                    "C2" = C2,
                    "B4" = B4,
                    "C4" = C4)
  save(experiment, file=sprintf("../experiments/results/%s.rdata",exp_names[j]))
}

#rm(list = ls())
for (exp_name in exp_names){
  assign(exp_name, get(load(file = sprintf("../experiments/results/%s.rdata",
                                           exp_name))))
}






















